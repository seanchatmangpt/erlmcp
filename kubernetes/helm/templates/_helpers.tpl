{{/* vim: set filetype=mustache: */}}
{{/*
Expand the name of the chart.
*/}}
{{- define "erlmcp.name" -}}
{{- default .Chart.Name .Values.global.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "erlmcp.fullname" -}}
{{- if .Values.global.fullnameOverride }}
{{- .Values.global.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.global.nameOverride }}
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
{{- define "erlmcp.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "erlmcp.labels" -}}
helm.sh/chart: {{ include "erlmcp.chart" . }}
{{ include "erlmcp.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- if .Values.global.additionalLabels }}
{{- toYaml .Values.global.additionalLabels | nindent 4 }}
{{- end }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "erlmcp.selectorLabels" -}}
app.kubernetes.io/name: {{ include "erlmcp.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "erlmcp.serviceAccountName" -}}
{{- if .Values.global.serviceAccount.create }}
{{- default (include "erlmcp.fullname" .) .Values.global.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.global.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
Cluster service name
*/}}
{{- define "erlmcp.clusterServiceName" -}}
{{- if .Values.cluster.discovery.kubernetes.enabled }}
{{- .Values.cluster.discovery.kubernetes.serviceName }}
{{- else }}
{{- include "erlmcp.fullname" . }}
{{- end }}
{{- end }}

{{/*
Config map name
*/}}
{{- define "erlmcp.configMapName" -}}
{{- include "erlmcp.fullname" . }}-config
{{- end }}

{{/*
Secret name
*/}}
{{- define "erlmcp.secretName" -}}
{{- include "erlmcp.fullname" . }}-secret
{{- end }}

{{/*
Ingress host
*/}}
{{- define "erlmcp.ingressHost" -}}
{{- .Values.ingress.hosts[0].host }}
{{- end }}

{{/*
Default image tag
*/}}
{{- define "erlmcp.imageTag" -}}
{{- if .Values.global.image.tag }}
{{- .Values.global.image.tag }}
{{- else }}
{{- .Chart.AppVersion }}
{{- end }}
{{- end }}

{{/*
CPU resource request
*/}}
{{- define "erlmcp.cpuRequest" -}}
{{- .Values.resources.core.requests.cpu | default "2" }}
{{- end }}

{{/*
Memory resource request
*/}}
{{- define "erlmcp.memoryRequest" -}}
{{- .Values.resources.core.requests.memory | default "4Gi" }}
{{- end }}

{{/*
CPU resource limit
*/}}
{{- define "erlmcp.cpuLimit" -}}
{{- .Values.resources.core.limits.cpu | default "4" }}
{{- end }}

{{/*
Memory resource limit
*/}}
{{- define "erlmcp.memoryLimit" -}}
{{- .Values.resources.core.limits.memory | default "8Gi" }}
{{- end }}

{{/*
Node name generation
*/}}
{{- define "erlmcp.nodeName" -}}
{{- include "erlmcp.fullname" . }}-{{ .Values.node.replicaCount | int | add 1 }}
{{- end }}

{{/*
Cluster cookie
*/}}
{{- define "erlmcp.clusterCookie" -}}
{{- .Values.cluster.cookie }}
{{- end }}

{{/*
Log level
*/}}
{{- define "erlmcp.logLevel" -}}
{{- .Values.monitoring.logging.level | default "info" }}
{{- end }}

{{/*
Log format
*/}}
{{- define "erlmcp.logFormat" -}}
{{- .Values.monitoring.logging.format | default "json" }}
{{- end }}

{{/*
Tolerance check for node selector
*/}}
{{- define "erlmcp.nodeTolerations" -}}
{{- range $key, $value := .Values.tolerations }}
- key: {{ $key }}
  operator: Exists
{{- range $key2, $value2 := $value }}
  {{ $key2 }}: {{ $value2 }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Node affinity rules
*/}}
{{- define "erlmcp.nodeAffinity" -}}
{{- if .Values.node.affinity.nodeAffinity.requiredDuringSchedulingIgnoredDuringExecution }}
requiredDuringSchedulingIgnoredDuringExecution:
{{- toYaml .Values.node.affinity.nodeAffinity.requiredDuringSchedulingIgnoredDuringExecution | nindent 4 }}
{{- end }}
{{- if .Values.node.affinity.nodeAffinity.preferredDuringSchedulingIgnoredDuringExecution }}
preferredDuringSchedulingIgnoredDuringExecution:
{{- toYaml .Values.node.affinity.nodeAffinity.preferredDuringSchedulingIgnoredDuringExecution | nindent 4 }}
{{- end }}
{{- end }}

{{/*
Pod anti-affinity rules
*/}}
{{- define "erlmcp.podAntiAffinity" -}}
{{- if .Values.node.affinity.podAntiAffinity.requiredDuringSchedulingIgnoredDuringExecution }}
requiredDuringSchedulingIgnoredDuringExecution:
{{- toYaml .Values.node.affinity.podAntiAffinity.requiredDuringSchedulingIgnoredDuringExecution | nindent 4 }}
{{- end }}
{{- if .Values.node.affinity.podAntiAffinity.preferredDuringSchedulingIgnoredDuringExecution }}
preferredDuringSchedulingIgnoredDuringExecution:
{{- toYaml .Values.node.affinity.podAntiAffinity.preferredDuringSchedulingIgnoredDuringExecution | nindent 4 }}
{{- end }}
{{- end }}

{{/*
Service annotations
*/}}
{{- define "erlmcp.serviceAnnotations" -}}
{{- if .Values.networking.annotations }}
{{- toYaml .Values.networking.annotations | nindent 4 }}
{{- end }}
{{- if .Values.services.http.annotations }}
{{- toYaml .Values.services.http.annotations | nindent 4 }}
{{- end }}
{{- end }}

{{/*
Merge config data
*/}}
{{- define "erlmcp.mergeConfigData" -}}
{{- $config := .Values.configMaps.main.data }}
{{- $env := .Values.configMaps.env.data }}
{{- if .Values.configMaps.main.enabled }}
config.json: |-
  {{- $config.config.json | nindent 2 }}
{{- end }}
{{- if .Values.configMaps.env.enabled }}
env.sh: |
  #!/bin/sh
  {{- range $key, $value := $env }}
  export {{ $key }}="{{ $value }}"
  {{- end }}
{{- end }}
{{- end }}

{{/*
Service mesh configuration
*/}}
{{- define "erlmcp.serviceMeshEnabled" -}}
{{- if .Values.global.serviceMesh.enabled }}
{{- if .Values.global.serviceMesh.istio.enabled }}
istio
{{- else if .Values.global.serviceMesh.linkerd.enabled }}
linkerd
{{- end }}
{{- else }}
none
{{- end }}
{{- end }}

{{/*
Resource requirements
*/}}
{{- define "erlmcp.resources" -}}
resources:
  requests:
    cpu: "{{ include "erlmcp.cpuRequest" . }}"
    memory: "{{ include "erlmcp.memoryRequest" . }}"
  limits:
    cpu: "{{ include "erlmcp.cpuLimit" . }}"
    memory: "{{ include "erlmcp.memoryLimit" . }}"
{{- if .Values.resources.core.extra }}
{{- toYaml .Values.resources.core.extra | nindent 2 }}
{{- end }}
{{- end }}

{{/*
Probes configuration
*/}}
{{- define "erlmcp.probes" -}}
{{- if .Values.probes.liveness.enabled }}
livenessProbe:
  httpGet:
    path: /health
    port: http
  initialDelaySeconds: {{ .Values.probes.liveness.initialDelaySeconds }}
  periodSeconds: {{ .Values.probes.liveness.periodSeconds }}
  timeoutSeconds: {{ .Values.probes.liveness.timeoutSeconds }}
  failureThreshold: {{ .Values.probes.liveness.failureThreshold }}
  successThreshold: {{ .Values.probes.liveness.successThreshold }}
{{- end }}
{{- if .Values.probes.readiness.enabled }}
readinessProbe:
  httpGet:
    path: /ready
    port: http
  initialDelaySeconds: {{ .Values.probes.readiness.initialDelaySeconds }}
  periodSeconds: {{ .Values.probes.readiness.periodSeconds }}
  timeoutSeconds: {{ .Values.probes.readiness.timeoutSeconds }}
  failureThreshold: {{ .Values.probes.readiness.failureThreshold }}
  successThreshold: {{ .Values.probes.readiness.successThreshold }}
{{- end }}
{{- if .Values.probes.startup.enabled }}
startupProbe:
  httpGet:
    path: /ready
    port: http
  initialDelaySeconds: {{ .Values.probes.startup.initialDelaySeconds }}
  periodSeconds: {{ .Values.probes.startup.periodSeconds }}
  timeoutSeconds: {{ .Values.probes.startup.timeoutSeconds }}
  failureThreshold: {{ .Values.probes.startup.failureThreshold }}
  successThreshold: {{ .Values.probes.startup.successThreshold }}
{{- end }}
{{- end }}

{{/*
Volumes configuration
*/}}
{{- define "erlmcp.volumes" -}}
volumes:
{{- if .Values.persistence.sessions.enabled }}
  - name: sessions-data
    persistentVolumeClaim:
      claimName: {{ include "erlmcp.fullname" . }}-sessions
{{- end }}
{{- if .Values.persistence.registry.enabled }}
  - name: registry-data
    persistentVolumeClaim:
      claimName: {{ include "erlmcp.fullname" . }}-registry
{{- end }}
{{- if .Values.persistence.config.enabled }}
  - name: config-data
    persistentVolumeClaim:
      claimName: {{ include "erlmcp.fullname" . }}-config
{{- end }}
{{- if .Values.secrets.enabled }}
  - name: erlmcp-secrets
    secret:
      secretName: {{ include "erlmcp.secretName" . }}
{{- end }}
{{- if .Values.configMaps.main.enabled }}
  - name: erlmcp-config
    configMap:
      name: {{ include "erlmcp.configMapName" . }}
{{- end }}
{{- if .Values.monitoring.otel.enabled }}
  - name: otel-config
    configMap:
      name: erlmcp-otel-collector-config
{{- end }}
{{- end }}

{{/*
Volume mounts
*/}}
{{- define "erlmcp.volumeMounts" -}}
volumeMounts:
{{- if .Values.persistence.sessions.enabled }}
  - name: sessions-data
    mountPath: {{ .Values.persistence.sessions.mountPath }}
    subPath: {{ .Values.persistence.sessions.subPath }}
{{- end }}
{{- if .Values.persistence.registry.enabled }}
  - name: registry-data
    mountPath: {{ .Values.persistence.registry.mountPath }}
    subPath: {{ .Values.persistence.registry.subPath }}
{{- end }}
{{- if .Values.persistence.config.enabled }}
  - name: config-data
    mountPath: {{ .Values.persistence.config.mountPath }}
    subPath: {{ .Values.persistence.config.subPath }}
{{- end }}
{{- if .Values.secrets.enabled }}
  - name: erlmcp-secrets
    mountPath: "/etc/erlmcp/secrets"
    readOnly: true
{{- end }}
{{- if .Values.configMaps.main.enabled }}
  - name: erlmcp-config
    mountPath: "/etc/erlmcp"
    readOnly: true
{{- end }}
{{- end }}