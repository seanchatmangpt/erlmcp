{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "erlmcp-enterprise.name" -}}
{{- default .Chart.Name .Values.global.cluster.name | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "erlmcp-enterprise.fullname" -}}
{{- if .Values.global.cluster.fullnameOverride }}
{{- .Values.global.cluster.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.global.cluster.name }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "erlmcp-enterprise.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "erlmcp-enterprise.labels" -}}
helm.sh/chart: {{ include "erlmcp-enterprise.chart" . }}
{{ include "erlmcp-enterprise.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- if .Values.global.cluster.environment }}
app.kubernetes.io/environment: {{ .Values.global.cluster.environment }}
{{- end }}
{{- if .Values.global.cluster.region }}
app.kubernetes.io/region: {{ .Values.global.cluster.region }}
{{- end }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "erlmcp-enterprise.selectorLabels" -}}
app.kubernetes.io/name: {{ include "erlmcp-enterprise.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "erlmcp-enterprise.serviceAccountName" -}}
{{- if .Values.security.rbac.create }}
{{- default (include "erlmcp-enterprise.fullname" .) .Values.security.rbac.serviceAccountName }}
{{- else }}
{{- default "default" .Values.security.rbac.serviceAccountName }}
{{- end }}
{{- end }}

{{/*
Generate Redis password
*/}}
{{- define "erlmcp-enterprise.redis.password" -}}
{{- if .Values.security.secrets.redis.existingPassword }}
{{ .Values.security.secrets.redis.existingPassword }}
{{- else if .Values.security.secrets.redis.create }}
{{- randAlphaNum 32 }}
{{- else }}
{{- default "default" .Values.security.secrets.redis.password }}
{{- end }}
{{- end }}

{{/*
Generate Database password
*/}}
{{- define "erlmcp-enterprise.db.password" -}}
{{- if .Values.security.secrets.db.existingPassword }}
{{ .Values.security.secrets.db.existingPassword }}
{{- else if .Values.security.secrets.db.create }}
{{- randAlphaNum 32 }}
{{- else }}
{{- default "default" .Values.security.secrets.db.password }}
{{- end }}
{{- end }}

{{/*
Generate JWT secret
*/}}
{{- define "erlmcp-enterprise.jwt.secret" -}}
{{- if .Values.security.secrets.jwt.existingSecret }}
{{ .Values.security.secrets.jwt.existingSecret }}
{{- else if .Values.security.secrets.jwt.create }}
{{- randAlphaNum 64 }}
{{- else }}
{{- default "default-jwt-secret" .Values.security.secrets.jwt.secret }}
{{- end }}
{{- end }}

{{/*
Get the proper storage class
*/}}
{{- define "erlmcp-enterprise.storageClass" -}}
{{- if .Values.storage.persistentVolumeClaim.storageClass }}
{{- .Values.storage.persistentVolumeClaim.storageClass }}
{{- else }}
{{- if .Values.storage.persistentVolumeClaim.defaultStorageClass }}
{{- .Values.storage.persistentVolumeClaim.defaultStorageClass }}
{{- else }}
{{- if .Capabilities.APIVersions.Has "storage.k8s.io/v1" }}
{{- (include "erlmcp-enterprise.storageClassLookup" .) | trim }}
{{- else }}
standard
{{- end }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Look up the storage class to use
*/}}
{{- define "erlmcp-enterprise.storageClassLookup" -}}
{{- range .Values.storage.persistentVolumeClaim.allowedClasses }}
{{- if . }}
{{- . | trim }}
{{- end }}
{{- break }}
{{- end }}
{{- end }}

{{/*
Check if we should enable Prometheus
*/}}
{{- define "erlmcp-enterprise.prometheus.enabled" -}}
{{- if and .Values.monitoring.prometheus.enabled (.Capabilities.APIVersions.Has "monitoring.coreos.com/v1") }}
true
{{- else }}
false
{{- end }}
{{- end }}

{{/*
Check if we should enable Grafana
*/}}
{{- define "erlmcp-enterprise.grafana.enabled" -}}
{{- if and .Values.monitoring.grafana.enabled (.Capabilities.APIVersions.Has "monitoring.coreos.com/v1") }}
true
{{- else }}
false
{{- end }}
{{- end }}

{{/*
Check if we should enable Network Policy
*/}}
{{- define "erlmcp-enterprise.networkPolicy.enabled" -}}
{{- if and .Values.security.networkPolicy.enabled (.Capabilities.APIVersions.Has "networking.k8s.io/v1") }}
true
{{- else }}
false
{{- end }}
{{- end }}

{{/*
Image pull secret template
*/}}
{{- define "imagePullSecret" -}}
{{- with .Values.image }}
{{- printf "{\"auths\":{\"%s\":{\"username\":\"%s\",\"password\":\"%s\",\"auth\":\"%s\"}}}" .registry .username .password (printf "%s:%s" .username .password | b64enc) | b64enc }}
{{- end }}
{{- end }}

{{/*
Docker image reference
*/}}
{{- define "erlmcp-enterprise.image" -}}
{{- $registry := .Values.image.registry | default "docker.io" -}}
{{- $repository := .Values.image.repository -}}
{{- $tag := .Values.image.tag | default .Chart.AppVersion -}}
{{- printf "%s/%s:%s" $registry $repository $tag }}
{{- end }}

{{/*
Check if autoscaling is enabled
*/}}
{{- define "erlmcp-enterprise.autoscaling.enabled" -}}
{{- if and .Values.autoscaling.enabled (.Capabilities.APIVersions.Has "autoscaling/v2") }}
true
{{- else }}
false
{{- end }}
{{- end }}

{{/*
Pod Disruption Budget API version
*/}}
{{- define "erlmcp-enterprise.pdb.apiVersion" -}}
{{- if .Capabilities.APIVersions.Has "policy/v1" }}
policy/v1
{{- else }}
policy/v1beta1
{{- end }}
{{- end }}

{{/*
Ingress API version
*/}}
{{- define "erlmcp-enterprise.ingress.apiVersion" -}}
{{- if .Capabilities.APIVersions.Has "networking.k8s.io/v1" }}
networking.k8s.io/v1
{{- else if .Capabilities.APIVersions.Has "networking.k8s.io/v1beta1" }}
networking.k8s.io/v1beta1
{{- else }}
extensions/v1beta1
{{- end }}
{{- end }}

{{/*
Get the namespace for deployment
*/}}
{{- define "erlmcp-enterprise.namespace" -}}
{{- if .Values.global.cluster.namespace }}
{{- .Values.global.cluster.namespace }}
{{- else }}
{{- .Release.Namespace }}
{{- end }}
{{- end }}

{{/*
ResourceQuota enabled check
*/}}
{{- define "erlmcp-enterprise.resourceQuota.enabled" -}}
{{- if and .Values.resourceQuota.enabled (.Capabilities.APIVersions.Has "v1") }}
true
{{- else }}
false
{{- end }}
{{- end }}

{{/*
LimitRange enabled check
*/}}
{{- define "erlmcp-enterprise.limitRange.enabled" -}}
{{- if and .Values.limitRange.enabled (.Capabilities.APIVersions.Has "v1") }}
true
{{- else }}
false
{{- end }}
{{- end }}

{{/*
Convert memory string to Mi for quota calculations
*/}}
{{- define "erlmcp-enterprise.memoryToMi" -}}
{{- $mem := . | toString -}}
{{- $value := 0 -}}
{{- $unit := "" -}}
{{- if hasSuffix "Ki" $mem -}}
{{- $value = trimSuffix "Ki" $mem | int -}}
{{- $unit = "Ki" -}}
{{- else if hasSuffix "Mi" $mem -}}
{{- $value = trimSuffix "Mi" $mem | int -}}
{{- $unit = "Mi" -}}
{{- else if hasSuffix "Gi" $mem -}}
{{- $value = trimSuffix "Gi" $mem | int -}}
{{- $unit = "Gi" -}}
{{- else if hasSuffix "Ti" $mem -}}
{{- $value = trimSuffix "Ti" $mem | int -}}
{{- $unit = "Ti" -}}
{{- else if hasSuffix "K" $mem -}}
{{- $value = trimSuffix "K" $mem | int -}}
{{- $unit = "K" -}}
{{- else if hasSuffix "M" $mem -}}
{{- $value = trimSuffix "M" $mem | int -}}
{{- $unit = "M" -}}
{{- else if hasSuffix "G" $mem -}}
{{- $value = trimSuffix "G" $mem | int -}}
{{- $unit = "G" -}}
{{- else if hasSuffix "T" $mem -}}
{{- $value = trimSuffix "T" $mem | int -}}
{{- $unit = "T" -}}
{{- else -}}
{{- $value = $mem | int -}}
{{- $unit = "" -}}
{{- end -}}
{{- if eq $unit "Ki" }}{{ div $value 1024 }}{{- else if eq $unit "Mi" }}{{ $value }}{{- else if eq $unit "Gi" }}{{ mul $value 1024 }}{{- else if eq $unit "Ti" }}{{ mul $value 1048576 }}{{- else if eq $unit "K" }}{{ div $value 1000 }}{{- else if eq $unit "M" }}{{ $value }}{{- else if eq $unit "G" }}{{ mul $value 1000 }}{{- else if eq $unit "T" }}{{ mul $value 1000000 }}{{- else }}0{{- end }}
{{- end }}

{{/*
Calculate quota usage percentage
*/}}
{{- define "erlmcp-enterprise.quotaPercentage" -}}
{{- $used := .used | default 0 -}}
{{- $hard := .hard | default 1 -}}
{{- mul (div $used $hard) 100 }}
{{- end }}
