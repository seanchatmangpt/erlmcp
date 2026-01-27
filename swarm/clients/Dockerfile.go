FROM golang:1.21-alpine AS builder

RUN apk add --no-cache git make ca-certificates

WORKDIR /app

COPY go.mod go.sum ./
RUN go mod download

COPY . .

# Build optimized binary
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
    -ldflags="-w -s" \
    -o mcp-client-simulator \
    ./cmd/main.go

FROM alpine:3.18

RUN apk add --no-cache ca-certificates curl

COPY --from=builder /app/mcp-client-simulator /usr/local/bin/

WORKDIR /app

EXPOSE 8888

ENV CLIENT_TYPE=normal \
    TARGET_SERVERS=erlmcp-server \
    CLIENT_COUNT=25 \
    MESSAGE_RATE=100 \
    DURATION_SECONDS=3600 \
    METRICS_ENABLED=true \
    METRICS_PORT=8888

HEALTHCHECK --interval=10s --timeout=5s --retries=3 \
    CMD curl -f http://localhost:8888/health || exit 1

CMD ["mcp-client-simulator"]
