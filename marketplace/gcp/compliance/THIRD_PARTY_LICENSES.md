# Third-Party License Attributions for erlmcp

## Overview

This document lists all third-party open-source software components included in erlmcp v3.0.0, along with their license attributions.

## Core Runtime Dependencies

### Erlang/OTP 28.3.1
- **License**: Apache License 2.0
- **Source**: https://github.com/erlang/otp
- **Copyright**: Ericsson AB
- **Description**: Programming language runtime used by erlmcp

### Alpine Linux 3.20 (Base Image)
- **License**: Alpine Linux License (GPL-like)
- **Source**: https://alpinelinux.org/
- **Description**: Minimal Linux distribution for container base

### OpenSSL 3.x
- **License**: Apache License 2.0
- **Source**: https://www.openssl.org/
- **Description**: Cryptographic library for TLS
- **Note**: Used for secure communication

## Application Dependencies (rebar3)

### Build Tools

#### rebar3
- **License**: Apache License 2.0
- **Source**: https://github.com/erlang/rebar3
- **Description**: Build tool for Erlang

### JSON Handling

#### jiffy
- **License**: Apache License 2.0
- **Source**: https://github.com/davisp/jiffy
- **Description**: JSON encoder/decoder for Erlang
- **Used by**: erlmcp_core JSON-RPC implementation

#### jsone
- **License**: MIT License
- **Source**: https://github.com/sile/jsone
- **Description**: Alternative JSON encoder
- **Used by**: Optional JSON processing

### HTTP/WebSocket

#### gun
- **License**: ISC License
- **Source**: https://github.com/ninenines/gun
- **Description**: HTTP/1.1, HTTP/2 and WebSocket client
- **Used by**: HTTP and WebSocket transports

#### cowlib
- **License**: ISC License
- **Source**: https://github.com/ninenines/cowlib
- **Description**: Support library for Websocket
- **Used by**: WebSocket transport

#### ranch
- **License**: ISC License
- **Source**: https://github.com/ninenines/ranch
- **Description**: Socket acceptor pool
- **Used by**: TCP transport

### Logging

#### lager
- **License**: Apache License 2.0
- **Source**: https://github.com/erlang-lager/lager
- **Description**: Structured logging framework
- **Used by**: Application logging

#### logger
- **License**: Apache License 2.0
- **Source**: Erlang/OTP included
- **Description**: OTP logger application
- **Used by**: Application logging

### Configuration

#### hocon
- **License**: Apache License 2.0
- **Source**: https://github.com/hocon/hocon-erlang
- **Description**: HOCON configuration file parser
- **Used by**: Configuration management

### Observability

#### prometheus.erl
- **License**: Apache License 2.0
- **Source**: https://github.com/deadtrickster/prometheus.erl
- **Description**: Prometheus instrumentation library
- **Used by**: Metrics export

#### opentelemetry-api
- **License**: Apache License 2.0
- **Source**: https://github.com/open-telemetry/opentelemetry-erlang-api
- **Description**: OpenTelemetry API for Erlang
- **Used by**: Distributed tracing

#### opentelemetry-sdk
- **License**: Apache License 2.0
- **Source**: https://github.com/open-telemetry/opentelemetry-erlang
- **Description**: OpenTelemetry SDK for Erlang
- **Used by**: Tracing implementation

### Testing

#### proper
- **License**: GPL-3.0-or-later
- **Source**: https://github.com/proper-testing/proper
- **Description**: Property-based testing tool
- **Used by**: Test suite only (not in production)

#### meck
- **License**: Apache License 2.0
- **Source**: https://github.com/eproxus/meck
- **Description**: Dynamic mocking library
- **Used by**: Test suite only (not in production)

#### hammock
- **License**: Apache License 2.0
- **Source**: https://github.com/ferd/hammock
- **Description**: Mock library for Elixir
- **Used by**: Test suite only (not in production)

### Data Storage

#### rocksdb
- **License**: Apache License 2.0 and GPL-2.0
- **Source**: https://github.com/facebook/rocksdb
- **Description**: Embedded persistent key-value store
- **Used by**: Optional ETS backup storage

### Security

#### public_key
- **License**: Apache License 2.0
- **Source**: Erlang/OTP included
- **Description**: Public key cryptography
- **Used by**: JWT and mTLS

#### ssl
- **License**: Apache License 2.0
- **Source**: Erlang/OTP included
- **Description**: TLS/SSL implementation
- **Used by**: Secure communication

### Clustering and Distribution

#### gproc
- **License**: MIT License
- **Source**: https://github.com/uwiger/gproc
- **Description**: Process registry for Erlang
- **Used by**: Cluster discovery

#### ra
- **License**: Apache License 2.0
- **Source**: https://github.com/rabbitmq/ra
- **Description**: Raft consensus algorithm
- **Used by**: Distributed consensus

#### syn
- **License**: MIT License
- **Source**: https://github.com/ulfarini/syn
- **Description**: Process registry and PubSub
- **Used by**: Cluster communication

## Container Images

### Distroless (Optional)
- **License**: Multiple (see below)
- **Source**: https://github.com/GoogleContainerTools/distroless
- **Description**: Minimal container images
- **Components**:
  - glibc: LGPL-2.1
  - openssl: Apache 2.0
  - ca-certificates: MPL-2.0

## Development Tools (Not in Production)

### Build and Test

#### dialyzer
- **License**: Apache License 2.0
- **Source**: Erlang/OTP included
- **Description**: Static analysis tool
- **Used by**: Development only

#### xref
- **License**: Apache License 2.0
- **Source**: Erlang/OTP included
- **Description**: Cross-reference analysis tool
- **Used by**: Development only

#### cover
- **License**: Apache License 2.0
- **Source**: Erlang/OTP included
- **Description**: Code coverage analysis
- **Used by**: Development only

## License Summaries

### Apache License 2.0

Most dependencies use the Apache License 2.0:
```
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

### MIT License

Several dependencies use the MIT License:
```
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### ISC License

Nine's NINE libraries use the ISC License:
```
Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```

## GPL Dependencies

### proper (Property-based Testing)

**License**: GPL-3.0-or-later
**Impact**: Used only in test suite, NOT linked into production code
**Isolation**: Test dependencies are not included in release artifacts

```
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
```

## Obtaining Source Code

### Container Image Sources

Source code for all components is available at:
- Main repository: https://github.com/banyan-platform/erlmcp
- Dependencies: Listed in `rebar.lock` with source URLs

### Written Offer

To obtain a copy of the source code for components distributed under
GPL or LGPL licenses, please send a request to:

- Email: source@banyan-platform.io
- Subject: Source Code Request for erlmcp

This offer is valid for 3 years from the date of distribution.

## Attribution Notice

This product includes software developed by:
- The Erlang/OTP Team at Ericsson AB
- The OpenSSL Project
- The Apache Software Foundation
- Five9n (ninenines) for gun, ranch, cowlib, and other libraries
- The OpenTelemetry community
- The Prometheus community

## Contact

For licensing questions:
- Email: legal@banyan-platform.io
- GitHub: https://github.com/banyan-platform/erlmcp/issues

## Version Information

This attribution file is for erlmcp version 3.0.0, released on 2024-01-01.
