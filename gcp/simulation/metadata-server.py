#!/usr/bin/env python3
"""
GCP Metadata Server Simulation
==============================
Simulates the GCE metadata server (169.254.169.254) for local development.
Applications can query this for instance metadata, service account tokens, etc.

Usage:
    gunicorn -b 0.0.0.0:80 metadata-server:app

Environment Variables:
    PROJECT_ID: GCP project ID (default: erlmcp-local)
    ZONE: GCE zone (default: us-central1-a)
    REGION: GCE region (default: us-central1)
    INSTANCE_NAME: Instance name (default: erlmcp-dev-instance)
    SERVICE_ACCOUNT: Service account email
"""

import os
import time
import json
import jwt
from datetime import datetime, timedelta
from flask import Flask, request, jsonify, Response

app = Flask(__name__)

# Configuration from environment
PROJECT_ID = os.environ.get('PROJECT_ID', 'erlmcp-local')
PROJECT_NUMBER = os.environ.get('PROJECT_NUMBER', '123456789012')
ZONE = os.environ.get('ZONE', 'us-central1-a')
REGION = os.environ.get('REGION', 'us-central1')
INSTANCE_NAME = os.environ.get('INSTANCE_NAME', 'erlmcp-dev-instance')
INSTANCE_ID = os.environ.get('INSTANCE_ID', '1234567890123456789')
SERVICE_ACCOUNT = os.environ.get(
    'SERVICE_ACCOUNT',
    f'erlmcp-service-account@{PROJECT_ID}.iam.gserviceaccount.com'
)
MACHINE_TYPE = os.environ.get('MACHINE_TYPE', 'e2-medium')

# JWT signing key (for local simulation only)
JWT_SECRET = os.environ.get('JWT_SECRET', 'local-dev-secret-key')


def require_metadata_header(f):
    """Decorator to require Metadata-Flavor: Google header."""
    def wrapper(*args, **kwargs):
        if request.headers.get('Metadata-Flavor') != 'Google':
            return Response(
                'Missing required header: Metadata-Flavor: Google',
                status=403,
                mimetype='text/plain'
            )
        return f(*args, **kwargs)
    wrapper.__name__ = f.__name__
    return wrapper


def text_response(content):
    """Return plain text response."""
    return Response(content, mimetype='text/plain')


def json_response(content):
    """Return JSON response."""
    return Response(json.dumps(content), mimetype='application/json')


# ============================================================================
# Root endpoint
# ============================================================================

@app.route('/computeMetadata/v1/')
@require_metadata_header
def metadata_root():
    """List available metadata endpoints."""
    return text_response('instance/\nproject/\n')


@app.route('/')
@require_metadata_header
def root():
    """Redirect to computeMetadata."""
    return text_response('computeMetadata/\n')


# ============================================================================
# Project metadata
# ============================================================================

@app.route('/computeMetadata/v1/project/')
@require_metadata_header
def project_root():
    return text_response('project-id\nnumeric-project-id\nattributes/\n')


@app.route('/computeMetadata/v1/project/project-id')
@require_metadata_header
def project_id():
    return text_response(PROJECT_ID)


@app.route('/computeMetadata/v1/project/numeric-project-id')
@require_metadata_header
def numeric_project_id():
    return text_response(PROJECT_NUMBER)


@app.route('/computeMetadata/v1/project/attributes/')
@require_metadata_header
def project_attributes():
    return text_response('cluster-name\ncluster-location\n')


@app.route('/computeMetadata/v1/project/attributes/<key>')
@require_metadata_header
def project_attribute(key):
    attributes = {
        'cluster-name': 'erlmcp-dev-cluster',
        'cluster-location': REGION,
        'enable-oslogin': 'TRUE',
    }
    if key in attributes:
        return text_response(attributes[key])
    return Response('Not found', status=404)


# ============================================================================
# Instance metadata
# ============================================================================

@app.route('/computeMetadata/v1/instance/')
@require_metadata_header
def instance_root():
    return text_response(
        'id\nname\nzone\nmachine-type\n'
        'hostname\nnetwork-interfaces/\n'
        'service-accounts/\nattributes/\ntags\n'
    )


@app.route('/computeMetadata/v1/instance/id')
@require_metadata_header
def instance_id():
    return text_response(INSTANCE_ID)


@app.route('/computeMetadata/v1/instance/name')
@require_metadata_header
def instance_name():
    return text_response(INSTANCE_NAME)


@app.route('/computeMetadata/v1/instance/zone')
@require_metadata_header
def instance_zone():
    return text_response(f'projects/{PROJECT_NUMBER}/zones/{ZONE}')


@app.route('/computeMetadata/v1/instance/machine-type')
@require_metadata_header
def machine_type():
    return text_response(f'projects/{PROJECT_NUMBER}/machineTypes/{MACHINE_TYPE}')


@app.route('/computeMetadata/v1/instance/hostname')
@require_metadata_header
def hostname():
    return text_response(f'{INSTANCE_NAME}.{ZONE}.c.{PROJECT_ID}.internal')


@app.route('/computeMetadata/v1/instance/tags')
@require_metadata_header
def tags():
    return json_response(['erlmcp', 'development', 'simulation'])


@app.route('/computeMetadata/v1/instance/attributes/')
@require_metadata_header
def instance_attributes():
    return text_response('cluster-name\ncluster-uid\n')


@app.route('/computeMetadata/v1/instance/attributes/<key>')
@require_metadata_header
def instance_attribute(key):
    attributes = {
        'cluster-name': 'erlmcp-dev-cluster',
        'cluster-uid': 'local-cluster-uid-12345',
        'created-by': 'simulation',
    }
    if key in attributes:
        return text_response(attributes[key])
    return Response('Not found', status=404)


# ============================================================================
# Network interfaces
# ============================================================================

@app.route('/computeMetadata/v1/instance/network-interfaces/')
@require_metadata_header
def network_interfaces():
    return text_response('0/\n')


@app.route('/computeMetadata/v1/instance/network-interfaces/0/')
@require_metadata_header
def network_interface_0():
    return text_response('ip\nnetwork\nsubnetwork\naccess-configs/\n')


@app.route('/computeMetadata/v1/instance/network-interfaces/0/ip')
@require_metadata_header
def network_interface_ip():
    return text_response('10.0.0.2')


@app.route('/computeMetadata/v1/instance/network-interfaces/0/network')
@require_metadata_header
def network_interface_network():
    return text_response(f'projects/{PROJECT_NUMBER}/networks/default')


# ============================================================================
# Service accounts
# ============================================================================

@app.route('/computeMetadata/v1/instance/service-accounts/')
@require_metadata_header
def service_accounts():
    return text_response(f'{SERVICE_ACCOUNT}/\ndefault/\n')


@app.route('/computeMetadata/v1/instance/service-accounts/default/')
@app.route(f'/computeMetadata/v1/instance/service-accounts/{SERVICE_ACCOUNT}/')
@require_metadata_header
def service_account_default():
    return text_response('aliases\nemail\nidentity\nscopes\ntoken\n')


@app.route('/computeMetadata/v1/instance/service-accounts/default/email')
@app.route(f'/computeMetadata/v1/instance/service-accounts/{SERVICE_ACCOUNT}/email')
@require_metadata_header
def service_account_email():
    return text_response(SERVICE_ACCOUNT)


@app.route('/computeMetadata/v1/instance/service-accounts/default/scopes')
@app.route(f'/computeMetadata/v1/instance/service-accounts/{SERVICE_ACCOUNT}/scopes')
@require_metadata_header
def service_account_scopes():
    scopes = [
        'https://www.googleapis.com/auth/cloud-platform',
        'https://www.googleapis.com/auth/devstorage.full_control',
        'https://www.googleapis.com/auth/logging.write',
        'https://www.googleapis.com/auth/monitoring.write',
        'https://www.googleapis.com/auth/pubsub',
        'https://www.googleapis.com/auth/sqlservice.admin',
    ]
    return json_response(scopes)


@app.route('/computeMetadata/v1/instance/service-accounts/default/token')
@app.route(f'/computeMetadata/v1/instance/service-accounts/{SERVICE_ACCOUNT}/token')
@require_metadata_header
def service_account_token():
    """Generate a simulated access token."""
    now = datetime.utcnow()
    expiry = now + timedelta(hours=1)

    # Create JWT payload (simulated Google access token)
    payload = {
        'iss': f'https://accounts.google.com',
        'azp': SERVICE_ACCOUNT,
        'aud': 'https://www.googleapis.com/',
        'sub': SERVICE_ACCOUNT,
        'email': SERVICE_ACCOUNT,
        'email_verified': True,
        'iat': int(now.timestamp()),
        'exp': int(expiry.timestamp()),
    }

    # Generate token
    token = jwt.encode(payload, JWT_SECRET, algorithm='HS256')

    return json_response({
        'access_token': token,
        'expires_in': 3600,
        'token_type': 'Bearer',
    })


@app.route('/computeMetadata/v1/instance/service-accounts/default/identity')
@app.route(f'/computeMetadata/v1/instance/service-accounts/{SERVICE_ACCOUNT}/identity')
@require_metadata_header
def service_account_identity():
    """Generate identity token for the requested audience."""
    audience = request.args.get('audience', 'https://erlmcp.local')

    now = datetime.utcnow()
    expiry = now + timedelta(hours=1)

    payload = {
        'iss': 'https://accounts.google.com',
        'aud': audience,
        'azp': SERVICE_ACCOUNT,
        'sub': SERVICE_ACCOUNT,
        'email': SERVICE_ACCOUNT,
        'email_verified': True,
        'iat': int(now.timestamp()),
        'exp': int(expiry.timestamp()),
    }

    token = jwt.encode(payload, JWT_SECRET, algorithm='HS256')
    return text_response(token)


# ============================================================================
# Health check
# ============================================================================

@app.route('/health')
def health():
    return json_response({'status': 'healthy', 'service': 'metadata-server'})


# ============================================================================
# Main
# ============================================================================

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=80, debug=True)
