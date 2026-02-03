# erlmcp v3 Deployment Notification Templates

**Version:** 3.0.0
**Last Updated:** 2026-02-02

This document contains all notification templates used throughout the deployment workflow.

---

## Template Variables

All templates support the following variables:

| Variable | Description | Example |
|----------|-------------|---------|
| `{{deployment_id}}` | Unique deployment identifier | `dep-1234567890` |
| `{{version}}` | Version being deployed | `3.0.0` |
| `{{environment}}` | Target environment | `production` |
| `{{actor}}` | User/service initiating | `user@example.com` |
| `{{commit_sha}}` | Git commit SHA | `abc123def456` |
| `{{commit_url}}` | GitHub commit URL | `https://github.com/...` |
| `{{workflow_url}}` | Workflow run URL | `https://github.com/...` |
| `{{timestamp}}` | ISO 8601 timestamp | `2026-02-02T12:00:00Z` |
| `{{duration}}` | Human-readable duration | `8m 32s` |
| `{{previous_version}}` | Previous deployed version | `2.9.5` |

---

## Slack Templates

### Deployment Started

```json
{
  "username": "erlmcp Deploy",
  "icon_emoji": ":rocket:",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": ":rocket: Deployment Started"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n{{environment}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Version:*\n{{version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Actor:*\n{{actor}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Duration:*\nEst. {{estimated_duration}}"
        }
      ]
    },
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Commit:* `{{commit_sha}}`\n*Workflow:* {{workflow_url}}"
      }
    },
    {
      "type": "actions",
      "elements": [
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "View Workflow"
          },
          "url": "{{workflow_url}}"
        },
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "Cancel Deployment"
          },
          "style": "danger",
          "url": "{{cancel_url}}"
        }
      ]
    }
  ]
}
```

### Deployment Completed

```json
{
  "username": "erlmcp Deploy",
  "icon_emoji": ":white_check_mark:",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": ":white_check_mark: Deployment Completed"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n{{environment}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Version:*\n{{version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Duration:*\n{{duration}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Health:*\nAll checks passed"
        }
      ]
    },
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Metrics:*\n• Error Rate: `0.05%`\n• P95 Latency: `245ms`\n• Throughput: `12,345 req/s`"
      }
    },
    {
      "type": "actions",
      "elements": [
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "View Dashboard"
          },
          "url": "{{dashboard_url}}"
        },
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "View Logs"
          },
          "url": "{{logs_url}}"
        }
      ]
    }
  ]
}
```

### Deployment Failed

```json
{
  "username": "erlmcp Deploy",
  "icon_emoji": ":x:",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": ":x: Deployment Failed"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n{{environment}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Version:*\n{{version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Stage:*\n{{failed_stage}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Error:*\n{{error_message}}"
        }
      ]
    },
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Rollback:* {{rollback_status}}\n*Logs:* {{workflow_url}}"
      }
    },
    {
      "type": "actions",
      "elements": [
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "View Logs"
          },
          "url": "{{workflow_url}}"
        },
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "Manual Rollback"
          },
          "style": "danger",
          "url": "{{rollback_url}}"
        }
      ]
    }
  ]
}
```

### Rollback Initiated

```json
{
  "username": "erlmcp Deploy",
  "icon_emoji": ":warning:",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": ":warning: Automatic Rollback Initiated"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n{{environment}}"
        },
        {
          "type": "mrkdwn",
          "text": "*From:*\n{{version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*To:*\n{{previous_version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Reason:*\n{{rollback_reason}}"
        }
      ]
    },
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Trigger:*\n{{trigger_condition}}\n*Estimated Duration:* {{estimated_duration}}"
      }
    },
    {
      "type": "context",
      "elements": [
        {
          "type": "mrkdwn",
          "text": "@oncall please investigate"
        }
      ]
    }
  ]
}
```

### Approval Required

```json
{
  "username": "erlmcp Deploy",
  "icon_emoji": ":memo:",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": ":memo: Approval Required"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n{{environment}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Version:*\n{{version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Gate:*\n{{gate_name}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Expires:*\n{{expires_at}}"
        }
      ]
    },
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Changes:*\n{{commit_summary}}"
      }
    },
    {
      "type": "actions",
      "elements": [
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "Approve"
          },
          "style": "primary",
          "url": "{{approve_url}}"
        },
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "Reject"
          },
          "style": "danger",
          "url": "{{reject_url}}"
        },
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "View Changes"
          },
          "url": "{{commit_url}}"
        }
      ]
    }
  ]
}
```

### Canary Warning

```json
{
  "username": "erlmcp Deploy",
  "icon_emoji": ":large_yellow_circle:",
  "blocks": [
    {
      "type": "header",
      "text": {
        "type": "plain_text",
        "text": ":large_yellow_circle: Canary Analysis Warning"
      }
    },
    {
      "type": "section",
      "fields": [
        {
          "type": "mrkdwn",
          "text": "*Environment:*\n{{environment}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Canary:*\n{{version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Baseline:*\n{{baseline_version}}"
        },
        {
          "type": "mrkdwn",
          "text": "*Traffic:*\n{{traffic_percentage}}%"
        }
      ]
    },
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "*Metrics:*\n• Error Rate: `{{error_rate}}%` (threshold: {{error_threshold}}%)\n• P95 Latency: `{{p95_latency}}ms` (threshold: {{latency_threshold}}ms)"
      }
    },
    {
      "type": "actions",
      "elements": [
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "View Canary"
          },
          "url": "{{canary_url}}"
        },
        {
          "type": "button",
          "text": {
            "type": "plain_text",
            "text": "Pause Rollout"
          },
          "style": "danger",
          "url": "{{pause_url}}"
        }
      ]
    }
  ]
}
```

---

## Microsoft Teams Templates

### Deployment Started

```json
{
  "@type": "MessageCard",
  "@context": "https://schema.org/extensions",
  "summary": "Deployment Started",
  "themeColor": "0078D7",
  "title": "Deployment Started",
  "sections": [
    {
      "facts": [
        {
          "name": "Environment",
          "value": "{{environment}}"
        },
        {
          "name": "Version",
          "value": "{{version}}"
        },
        {
          "name": "Actor",
          "value": "{{actor}}"
        },
        {
          "name": "Estimated Duration",
          "value": "{{estimated_duration}}"
        }
      ]
    },
    {
      "activityTitle": "Deployment initiated",
      "activitySubtitle": "{{timestamp}}",
      "activityImage": "https://erlmcp.io/icons/deploy.png"
    }
  ],
  "potentialAction": [
    {
      "@type": "OpenUri",
      "name": "View Workflow",
      "targets": [
        {
          "os": "default",
          "uri": "{{workflow_url}}"
        }
      ]
    }
  ]
}
```

### Deployment Completed

```json
{
  "@type": "MessageCard",
  "@context": "https://schema.org/extensions",
  "summary": "Deployment Completed",
  "themeColor": "00FF00",
  "title": "Deployment Completed Successfully",
  "sections": [
    {
      "facts": [
        {
          "name": "Environment",
          "value": "{{environment}}"
        },
        {
          "name": "Version",
          "value": "{{version}}"
        },
        {
          "name": "Duration",
          "value": "{{duration}}"
        },
        {
          "name": "Health Checks",
          "value": "All Passed"
        }
      ]
    }
  ],
  "potentialAction": [
    {
      "@type": "OpenUri",
      "name": "View Dashboard",
      "targets": [
        {
          "os": "default",
          "uri": "{{dashboard_url}}"
        }
      ]
    }
  ]
}
```

### Deployment Failed

```json
{
  "@type": "MessageCard",
  "@context": "https://schema.org/extensions",
  "summary": "Deployment Failed",
  "themeColor": "FF0000",
  "title": "Deployment Failed",
  "sections": [
    {
      "facts": [
        {
          "name": "Environment",
          "value": "{{environment}}"
        },
        {
          "name": "Version",
          "value": "{{version}}"
        },
        {
          "name": "Failed Stage",
          "value": "{{failed_stage}}"
        },
        {
          "name": "Error",
          "value": "{{error_message}}"
        }
      ]
    },
    {
      "text": "Rollback status: {{rollback_status}}"
    }
  ],
  "potentialAction": [
    {
      "@type": "OpenUri",
      "name": "View Logs",
      "targets": [
        {
          "os": "default",
          "uri": "{{workflow_url}}"
        }
      ]
    },
    {
      "@type": "OpenUri",
      "name": "Initiate Rollback",
      "targets": [
        {
          "os": "default",
          "uri": "{{rollback_url}}"
        }
      ]
    }
  ]
}
```

---

## PagerDuty Templates

### Critical Incident

```json
{
  "routing_key": "{{PAGERDUTY_INTEGRATION_KEY}}",
  "event_action": "trigger",
  "payload": {
    "summary": "Deployment Failed: {{version}} to {{environment}}",
    "severity": "critical",
    "source": "erlmcp-deploy",
    "timestamp": "{{timestamp}}",
    "component": "deployment",
    "group": "production",
    "class": "deployment_failure",
    "custom_details": {
      "deployment_id": "{{deployment_id}}",
      "version": "{{version}}",
      "environment": "{{environment}}",
      "failed_stage": "{{failed_stage}}",
      "error_message": "{{error_message}}",
      "workflow_url": "{{workflow_url}}",
      "rollback_initiated": "{{rollback_status}}"
    }
  },
  "dedup_key": "{{deployment_id}}",
  "images": [
    {
      "src": "https://erlmcp.io/icons/critical.png",
      "alt": "Critical Incident"
    }
  ],
  "links": [
    {
      "href": "{{workflow_url}}",
      "text": "View Workflow"
    },
    {
      "href": "{{rollback_url}}",
      "text": "Initiate Rollback"
    }
  ]
}
```

### High Priority Alert

```json
{
  "routing_key": "{{PAGERDUTY_INTEGRATION_KEY}}",
  "event_action": "trigger",
  "payload": {
    "summary": "Canary Warning: Error rate elevated during rollout",
    "severity": "warning",
    "source": "erlmcp-deploy",
    "timestamp": "{{timestamp}}",
    "component": "canary",
    "group": "production",
    "class": "canary_warning",
    "custom_details": {
      "deployment_id": "{{deployment_id}}",
      "version": "{{version}}",
      "baseline_version": "{{baseline_version}}",
      "error_rate": "{{error_rate}}",
      "error_threshold": "{{error_threshold}}",
      "p95_latency": "{{p95_latency}}",
      "latency_threshold": "{{latency_threshold}}"
    }
  },
  "dedup_key": "canary-{{deployment_id}}"
}
```

---

## Email Templates

### Deployment Started

```html
<!DOCTYPE html>
<html>
<head>
    <style>
        body { font-family: Arial, sans-serif; }
        .header { background: #0078D7; color: white; padding: 20px; }
        .content { padding: 20px; }
        .footer { color: #666; font-size: 12px; padding: 20px; }
        .button { background: #0078D7; color: white; padding: 10px 20px; text-decoration: none; border-radius: 5px; }
        .field { margin: 10px 0; }
        .label { font-weight: bold; }
    </style>
</head>
<body>
    <div class="header">
        <h1>:rocket: Deployment Started</h1>
    </div>
    <div class="content">
        <div class="field">
            <span class="label">Environment:</span>
            <span>{{environment}}</span>
        </div>
        <div class="field">
            <span class="label">Version:</span>
            <span>{{version}}</span>
        </div>
        <div class="field">
            <span class="label">Actor:</span>
            <span>{{actor}}</span>
        </div>
        <div class="field">
            <span class="label">Estimated Duration:</span>
            <span>{{estimated_duration}}</span>
        </div>
        <p>
            <a href="{{workflow_url}}" class="button">View Workflow</a>
        </p>
    </div>
    <div class="footer">
        <p>Deployment ID: {{deployment_id}}</p>
        <p>Started at: {{timestamp}}</p>
    </div>
</body>
</html>
```

### Deployment Completed

```html
<!DOCTYPE html>
<html>
<head>
    <style>
        body { font-family: Arial, sans-serif; }
        .header { background: #00CC00; color: white; padding: 20px; }
        .metrics { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin: 20px 0; }
        .metric { padding: 10px; background: #f5f5f5; border-radius: 5px; }
        .button { background: #00CC00; color: white; padding: 10px 20px; text-decoration: none; border-radius: 5px; }
    </style>
</head>
<body>
    <div class="header">
        <h1>:white_check_mark: Deployment Completed</h1>
    </div>
    <div class="content">
        <p>Deployment of <strong>{{version}}</strong> to <strong>{{environment}}</strong> completed successfully in <strong>{{duration}}</strong>.</p>
        <div class="metrics">
            <div class="metric">
                <h3>Error Rate</h3>
                <p>{{error_rate}}%</p>
            </div>
            <div class="metric">
                <h3>P95 Latency</h3>
                <p>{{p95_latency}}ms</p>
            </div>
            <div class="metric">
                <h3>Throughput</h3>
                <p>{{throughput}} req/s</p>
            </div>
            <div class="metric">
                <h3>Health</h3>
                <p>All checks passed</p>
            </div>
        </div>
        <p>
            <a href="{{dashboard_url}}" class="button">View Dashboard</a>
            <a href="{{logs_url}}" class="button">View Logs</a>
        </p>
    </div>
</body>
</html>
```

### Deployment Failed

```html
<!DOCTYPE html>
<html>
<head>
    <style>
        body { font-family: Arial, sans-serif; }
        .header { background: #CC0000; color: white; padding: 20px; }
        .error-box { background: #ffeeee; border-left: 4px solid #CC0000; padding: 15px; margin: 20px 0; }
        .button { background: #CC0000; color: white; padding: 10px 20px; text-decoration: none; border-radius: 5px; }
    </style>
</head>
<body>
    <div class="header">
        <h1>:x: Deployment Failed</h1>
    </div>
    <div class="content">
        <p>Deployment of <strong>{{version}}</strong> to <strong>{{environment}}</strong> failed at stage: <strong>{{failed_stage}}</strong></p>
        <div class="error-box">
            <h3>Error Details</h3>
            <p>{{error_message}}</p>
        </div>
        <p><strong>Rollback Status:</strong> {{rollback_status}}</p>
        <p>
            <a href="{{workflow_url}}" class="button">View Logs</a>
            <a href="{{rollback_url}}" class="button">Initiate Rollback</a>
        </p>
    </div>
</body>
</html>
```

---

## Webhook Templates

### Generic Webhook Payload

```json
{
  "event": "deployment.started",
  "timestamp": "{{timestamp}}",
  "deployment": {
    "id": "{{deployment_id}}",
    "version": "{{version}}",
    "environment": "{{environment}}",
    "actor": "{{actor}}",
    "status": "in_progress"
  },
  "git": {
    "commit_sha": "{{commit_sha}}",
    "commit_url": "{{commit_url}}",
    "branch": "{{branch}}"
  },
  "urls": {
    "workflow": "{{workflow_url}}",
    "dashboard": "{{dashboard_url}}",
    "logs": "{{logs_url}}"
  },
  "metadata": {
    "estimated_duration": "{{estimated_duration}}",
    "canary_enabled": {{canary_enabled}},
    "rollback_enabled": {{rollback_enabled}}
  }
}
```

### Datadog Event Format

```json
{
  "title": "Deployment {{deployment_status}}: {{version}} to {{environment}}",
  "text": "Deployment {{deployment_id}} {{deployment_status}} at {{timestamp}}",
  "alert_type": "{{alert_level}}",
  "tags": [
    "environment:{{environment}}",
    "version:{{version}}",
    "actor:{{actor}}",
    "deployment_id:{{deployment_id}}"
  ],
  "source_type_name": "erlmcp",
  "priority": "{{priority}}"
}
```

---

## Status Page Updates

### Maintenance Window

```markdown
## Scheduled Maintenance

**Start:** {{start_time}}
**End:** {{end_time}}
**Affected Services:** {{affected_services}}
**Impact:** {{impact_description}}

We will be deploying version {{version}} to {{environment}}. During this time, you may experience brief interruptions. Updates will be posted here.

---

*Last Updated: {{timestamp}}*
```

### Incident Update

```markdown
## {{incident_title}}

**Status:** {{status}}
**Started:** {{start_time}}
**Impact:** {{impact_level}}

{{incident_description}}

### Updates

* {{update_time}} - {{update_message}}
* {{update_time}} - {{update_message}}

---

*Next Update: {{next_update_time}}*
*View Details: {{incident_url}}*
```

---

## Template Configuration

The notification system is configured in `config/notification.yml`:

```yaml
templates:
  slack:
    deployment_started: "templates/slack/deployment-started.json"
    deployment_completed: "templates/slack/deployment-completed.json"
    deployment_failed: "templates/slack/deployment-failed.json"
    rollback_initiated: "templates/slack/rollback-initiated.json"
    approval_required: "templates/slack/approval-required.json"
    canary_warning: "templates/slack/canary-warning.json"

  teams:
    deployment_started: "templates/teams/deployment-started.json"
    deployment_completed: "templates/teams/deployment-completed.json"
    deployment_failed: "templates/teams/deployment-failed.json"

  pagerduty:
    critical: "templates/pagerduty/critical.json"
    warning: "templates/pagerduty/warning.json"

  email:
    deployment_started: "templates/email/deployment-started.html"
    deployment_completed: "templates/email/deployment-completed.html"
    deployment_failed: "templates/email/deployment-failed.html"

variables:
  global:
    - deployment_id
    - version
    - environment
    - actor
    - timestamp
    - workflow_url

  git:
    - commit_sha
    - commit_url
    - branch
    - tag

  deployment:
    - duration
    - failed_stage
    - error_message
    - rollback_status

  canary:
    - traffic_percentage
    - error_rate
    - error_threshold
    - p95_latency
    - latency_threshold
    - baseline_version
```
