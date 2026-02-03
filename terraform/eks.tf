# EKS Cluster
resource "aws_eks_cluster" "cluster" {
  name     = local.cluster_identifier
  role_arn = aws_iam_role.eks_cluster.arn

  vpc_config {
    subnet_ids         = aws_subnet.private[*].id
    security_group_ids = [aws_security_group.cluster.id]
  }

  kubernetes_network_config {
    endpoint_private_access = false
    endpoint_public_access  = true
  }

  enabled_cluster_log_types = ["api", "audit"]

  depends_on = [
    aws_iam_role_policy_attachment.cluster_AmazonEKSClusterPolicy,
    aws_iam_role_policy_attachment.cluster_AmazonEKSVPCResourceController
  ]

  tags = local.tags
}

# EKS Cluster IAM Policy Attachment
resource "aws_iam_role_policy_attachment" "cluster_AmazonEKSVPCResourceController" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSVPCResourceController"
  role       = aws_iam_role.eks_cluster.name
}

# EKS Node Group
resource "aws_eks_node_group" "nodes" {
  cluster_name    = aws_eks_cluster.cluster.name
  node_group_name = var.node_group_name
  node_role_arn   = aws_iam_role.nodes.arn
  subnet_ids      = aws_subnet.private[*].id

  scaling_config {
    desired_size = var.desired_nodes
    max_size     = var.max_nodes
    min_size     = var.min_nodes
  }

  update_config {
    max_unavailable = var.max_unavailable
    max Surge       = var.max_surge
  }

  launch_type = "ON_DEMAND"

  instance_types = [var.instance_type]

  tags = local.tags

  depends_on = [
    aws_iam_role_policy_attachment.nodes_AmazonEKSWorkerNodePolicy,
    aws_iam_role_policy_attachment.nodes_AmazonEKS_CNI_Policy,
    aws_iam_role_policy_attachment.nodes_AmazonEC2ContainerRegistryReadOnly,
    aws_eks_cluster.cluster
  ]
}

# Node group autoscaling
resource "aws_autoscaling_group" "nodes" {
  name                 = "${var.environment}-eks-node-group"
  desired_capacity     = var.desired_nodes
  max_size             = var.max_nodes
  min_size             = var.min_nodes
  vpc_zone_identifier  = aws_subnet.private[*].id
  target_group_arns    = [aws_lb_target_group.main[0].arn]

  launch_template {
    id      = aws_launch_template.nodes.id
    version = "$Latest"
  }

  tag {
    key                 = "Name"
    value               = "erlmcp-${var.environment}-node"
    propagate_at_launch = true
  }

  tag {
    key                 = "kubernetes.io/cluster/${local.cluster_identifier}"
    value               = "owned"
    propagate_at_launch = true
  }

  lifecycle {
    ignore_changes = [load_balancers, target_group_arns]
  }
}

# Launch template for nodes
resource "aws_launch_template" "nodes" {
  name_prefix   = "erlmcp-${var.environment}-node"
  image_id      = data.aws_ami.eks_node.id
  instance_type = var.instance_type
  key_name      = "erlmcp-${var.environment}-key"

  network_interfaces {
    associate_public_ip_address = false
    delete_on_termination       = true
    security_groups             = [aws_security_group.nodes.id]
    subnet_id                   = aws_subnet.private[0].id
  }

  tag_specifications {
    resource_type = "instance"
    tags = merge(local.tags, {
      Name = "erlmcp-${var.environment}-node"
    })
  }

  user_data = base64encode(<<-EOT
    #!/bin/bash
    set -ex
    /etc/eks/bootstrap.sh ${local.cluster_identifier} \
      --b64-cluster-ca ${aws_eks_cluster.cluster.certificate_authority[0].data} \
      --apiserver-endpoint ${aws_eks_cluster.cluster.endpoint} \
      --kubelet-extra-args '--node-ip=$(curl http://169.254.169.254/latest/meta-data/local-ipv4)'
  EOT)

  depends_on = [aws_eks_cluster.cluster]
}

# EKS node group IAM OIDC provider
resource "aws_iam_openid_connect_provider" "eks" {
  client_id_list  = ["sts.amazonaws.com"]
  thumbprint_list = [data.tls_certificate.eks.certificates[0].sha1_fingerprint]
  url             = aws_eks_cluster.cluster.identity[0].oidc[0].issuer

  tags = local.tags
}

# Kubeconfig output
output "kubeconfig" {
  value = <<-EOT
    apiVersion: v1
    clusters:
    - cluster:
        server: ${aws_eks_cluster.cluster.endpoint}
        certificate-authority-data: ${aws_eks_cluster.cluster.certificate_authority[0].data}
      name: ${local.cluster_identifier}
    contexts:
    - context:
        cluster: ${local.cluster_identifier}
        user: ${local.cluster_identifier}
      name: ${local.cluster_identifier}
    current-context: ${local.cluster_identifier}
    kind: Config
    users:
    - name: ${local.cluster_identifier}
      user:
        exec:
          apiVersion: client.authentication.k8s.io/v1beta1
          args:
          - eks
          - get-token
          - --cluster-name
          - ${local.cluster_identifier}
          - --region
          - ${var.aws_region}
          command: aws
  EOT
}

# EKS endpoint
output "cluster_endpoint" {
  value = aws_eks_cluster.cluster.endpoint
}

# EKS cluster ID
output "cluster_id" {
  value = aws_eks_cluster.cluster.id
}