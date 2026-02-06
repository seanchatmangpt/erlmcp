# EKS Cluster with enhanced configuration
resource "aws_eks_cluster" "cluster" {
  name     = local.cluster_identifier
  role_arn = aws_iam_role.eks_cluster.arn
  version  = var.eks_version

  vpc_config {
    subnet_ids              = aws_subnet.private[*].id
    security_group_ids      = [aws_security_group.cluster.id]
    endpoint_private_access = true
    endpoint_public_access  = true
    public_access_cidrs     = var.cluster_endpoint_public_access_cidrs
  }

  kubernetes_network_config {
    service_ipv4_cidr = var.cluster_service_ipv4_cidr
    ip_family         = "ipv4"
  }

  enabled_cluster_log_types = ["api", "audit", "authenticator", "controllerManager", "scheduler"]

  encryption_config {
    provider {
      key_arn = aws_kms_key.eks.arn
    }
    resources = ["secrets"]
  }

  depends_on = [
    aws_iam_role_policy_attachment.cluster_AmazonEKSClusterPolicy,
    aws_iam_role_policy_attachment.cluster_AmazonEKSVPCResourceController,
    aws_cloudwatch_log_group.eks_cluster
  ]

  tags = local.tags
}

# CloudWatch log group for EKS cluster logs
resource "aws_cloudwatch_log_group" "eks_cluster" {
  name              = "/aws/eks/${local.cluster_identifier}/cluster"
  retention_in_days = 30
  kms_key_id        = aws_kms_key.eks.arn

  tags = local.tags
}

# KMS key for EKS encryption
resource "aws_kms_key" "eks" {
  description             = "EKS cluster encryption key for ${local.cluster_identifier}"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  tags = merge(local.tags, {
    Name = "${local.cluster_identifier}-eks-key"
  })
}

resource "aws_kms_alias" "eks" {
  name          = "alias/${local.cluster_identifier}-eks"
  target_key_id = aws_kms_key.eks.key_id
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
  }

  capacity_type = "ON_DEMAND"

  instance_types = [var.instance_type]

  tags = local.tags

  depends_on = [
    aws_iam_role_policy_attachment.nodes_AmazonEKSWorkerNodePolicy,
    aws_iam_role_policy_attachment.nodes_AmazonEKS_CNI_Policy,
    aws_iam_role_policy_attachment.nodes_AmazonEC2ContainerRegistryReadOnly,
    aws_eks_cluster.cluster
  ]
}

# Launch template for custom node configurations (if needed)
resource "aws_launch_template" "nodes" {
  name_prefix   = "erlmcp-${var.environment}-node-"
  image_id      = data.aws_ami.eks_node.id
  instance_type = var.instance_type

  vpc_security_group_ids = [aws_security_group.nodes.id]

  block_device_mappings {
    device_name = "/dev/xvda"

    ebs {
      volume_size           = 100
      volume_type           = "gp3"
      iops                  = 3000
      throughput            = 125
      encrypted             = true
      kms_key_id            = aws_kms_key.eks.arn
      delete_on_termination = true
    }
  }

  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
    instance_metadata_tags      = "enabled"
  }

  monitoring {
    enabled = true
  }

  tag_specifications {
    resource_type = "instance"
    tags = merge(local.tags, {
      Name = "erlmcp-${var.environment}-node"
    })
  }

  tag_specifications {
    resource_type = "volume"
    tags = merge(local.tags, {
      Name = "erlmcp-${var.environment}-node-volume"
    })
  }

  user_data = base64encode(templatefile("${path.module}/user_data/bootstrap.sh.tpl", {
    cluster_name        = local.cluster_identifier
    cluster_endpoint    = aws_eks_cluster.cluster.endpoint
    cluster_ca_data     = aws_eks_cluster.cluster.certificate_authority[0].data
    bootstrap_extra_args = "--use-max-pods false --kubelet-extra-args '--max-pods=110'"
  }))

  lifecycle {
    create_before_destroy = true
  }

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