{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "erlmcp.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create a default fully qualified app name.
*/}}
{{- define "erlmcp.fullname" -}}
{{- if .Values.fullnameOverride -}}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" -}}
{{- else -}}
{{- $name := default .Chart.Name .Values.nameOverride -}}
{{- if contains $name .Release.Name -}}
{{- .Release.Name | trunc 63 | trimSuffix "-" -}}
{{- else -}}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" -}}
{{- end -}}
{{- end -}}
{{- end -}}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "erlmcp.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Common labels
*/}}
{{- define "erlmcp.labels" -}}
helm.sh/chart: {{ include "erlmcp.chart" . }}
{{- if .Chart.AppVersion -}}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end -}}
{{- if .Values.global.environment -}}
app.kubernetes.io/environment: {{ .Values.global.environment | quote }}
{{- end -}}
app.kubernetes.io/managed-by: {{ .Release.Service | quote }}
{{- include "erlmcp.selectorLabels" . }}
{{- with .Values.erlmcp.labels }}
{{- toYaml . | nindent 0 }}
{{- end }}
{{- end -}}

{{/*
Selector labels
*/}}
{{- define "erlmcp.selectorLabels" -}}
app.kubernetes.io/name: {{ include "erlmcp.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end -}}

{{/*
Create the name of the service account to use
*/}}
{{- define "erlmcp.serviceAccountName" -}}
{{- if .Values.rbac.serviceAccount.create -}}
{{- default (include "erlmcp.fullname" .) .Values.rbac.serviceAccount.name -}}
{{- else -}}
{{- default "default" -}}
{{- end -}}
{{- end -}}

{{/*
Create the name of the configmap to use
*/}}
{{- define "erlmcp.configMapName" -}}
{{- printf "%s-config" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create the name of the secret to use
*/}}
{{- define "erlmcp.secretName" -}}
{{- printf "%s-tls" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Create the name of the persistent volume claim to use
*/}}
{{- define "erlmcp.pvcName" -}}
{{- printf "%s-data" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
ServiceMonitor name
*/}}
{{- define "erlmcp.serviceMonitorName" -}}
{{- printf "%s-monitor" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
HPA name
*/}}
{{- define "erlmcp.hpaName" -}}
{{- printf "%s-hpa" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
PDB name
*/}}
{{- define "erlmcp.pdbName" -}}
{{- printf "%s-pdb" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Priority class name
*/}}
{{- define "erlmcp.priorityClassName" -}}
{{- if .Values.erlmcp.priorityClassName -}}
{{- .Values.erlmcp.priorityClassName -}}
{{- else -}}
{{- default "high-priority" -}}
{{- end -}}
{{- end -}}

{{/*
NetworkPolicy name
*/}}
{{- define "erlmcp.networkPolicyName" -}}
{{- printf "%s-network-policy" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Role name
*/}}
{{- define "erlmcp.clusterRoleName" -}}
{{- printf "%s-role" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
RoleBinding name
*/}}
{{- define "erlmcp.clusterRoleBindingName" -}}
{{- printf "%s-rolebinding" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
RBAC ServiceAccount name
*/}}
{{- define "erlmcp.rbacServiceAccountName" -}}
{{- if .Values.rbac.serviceAccount.create -}}
{{- default (include "erlmcp.fullname" .) .Values.rbac.serviceAccount.name -}}
{{- else -}}
{{- default "default" -}}
{{- end -}}
{{- end -}}

{{/*
TLS secret name
*/}}
{{- define "erlmcp.tlsSecretName" -}}
{{- printf "%s-tls" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Auth secret name
*/}}
{{- define "erlmcp.authSecretName" -}}
{{- printf "%s-auth" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Sessions PVC name
*/}}
{{- define "erlmcp.sessionsPVCName" -}}
{{- printf "%s-sessions" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Logs PVC name
*/}}
{{- define "erlmcp.logsPVCName" -}}
{{- printf "%s-logs" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Vault secret name
*/}}
{{- define "erlmcp.vaultSecretName" -}}
{{- printf "%s-vault" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
AWS Secrets Manager name
*/}}
{{- define "erlmcp.awsSecretsManagerName" -}}
{{- printf "%s-aws-secrets" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Backup name
*/}}
{{- define "erlmcp.backupName" -}}
{{- printf "%s-backup" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Recovery name
*/}}
{{- define "erlmcp.recoveryName" -}}
{{- printf "%s-recovery" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Disaster recovery name
*/}}
{{- define "erlmcp.disasterRecoveryName" -}}
{{- printf "%s-dr" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Compliance name
*/}}
{{- define "erlmcp.complianceName" -}}
{{- printf "%s-compliance" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Logging name
*/}}
{{- define "erlmcp.loggingName" -}}
{{- printf "%s-logging" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Monitoring name
*/}}
{{- define "erlmcp.monitoringName" -}}
{{- printf "%s-monitoring" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Security name
*/}}
{{- define "erlmcp.securityName" -}}
{{- printf "%s-security" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
Service name
*/}}
{{- define "erlmcp.serviceName" -}}
{{- include "erlmcp.fullname" .}}
{{- end -}}

{{/*
Ingress name
*/}}
{{- define "erlmcp.ingressName" -}}
{{- printf "%s-ingress" (include "erlmcp.fullname" .) | trunc 63 | trimSuffix "-" -}}
{{- end -}}

{{/*
mTLS enabled check
*/}}
{{- define "erlmcp.mtls.enabled" -}}
{{- if .Values.erlmcp.config.security.mTLS.enabled -}}
true
{{- else -}}
false
{{- end -}}
{{- end -}}

{{/*
TLS version helper
*/}}
{{- define "erlmcp.tls-version" -}}
{{- .Values.tls.minVersion | default "1.3" -}}
{{- end -}}

{{/*
TLS enabled check
*/}}
{{- define "erlmcp.tls.enabled" -}}
{{- .Values.tls.enabled | default "false" -}}
{{- end -}}

{{/*
Cert Manager enabled check
*/}}
{{- define "erlmcp.certManager.enabled" -}}
{{- .Values.tls.certManager.enabled | default "false" -}}
{{- end -}}

{{/*
Cert Issuer name
*/}}
{{- define "erlmcp.certIssuer.name" -}}
{{- .Values.tls.certManager.issuer.name | default "erlmcp-issuer" -}}
{{- end -}}

{{/*
Cert Issuer kind
*/}}
{{- define "erlmcp.certIssuer.kind" -}}
{{- .Values.tls.certManager.issuer.kind | default "Issuer" -}}
{{- end -}}
