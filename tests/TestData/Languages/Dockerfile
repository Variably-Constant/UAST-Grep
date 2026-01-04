# Dockerfile Test File for UAST-Grep
# Tests: instructions, multi-stage builds, arguments, environment

# Build arguments
ARG BASE_IMAGE=mcr.microsoft.com/dotnet/sdk:8.0
ARG BUILD_CONFIGURATION=Release
ARG APP_VERSION=1.0.0

# ============================================================================
# Stage 1: Build stage
# ============================================================================
FROM ${BASE_IMAGE} AS build

# Labels (OCI standard)
LABEL org.opencontainers.image.title="UAST-Grep Test"
LABEL org.opencontainers.image.description="Dockerfile test file for parser testing"
LABEL org.opencontainers.image.version="${APP_VERSION}"
LABEL org.opencontainers.image.authors="Test Author <test@example.com>"
LABEL org.opencontainers.image.source="https://github.com/example/UAST-Grep"
LABEL maintainer="Test Author"

# Environment variables
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1 \
    DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1 \
    NUGET_XMLDOC_MODE=skip \
    APP_NAME=UAST-Grep \
    APP_HOME=/app

# Working directory
WORKDIR ${APP_HOME}

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    git \
    jq \
    unzip \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user
RUN groupadd --gid 1000 appgroup \
    && useradd --uid 1000 --gid 1000 --create-home appuser \
    && chown -R appuser:appgroup ${APP_HOME}

# Copy dependency files first (for layer caching)
COPY --chown=appuser:appgroup *.csproj ./
COPY --chown=appuser:appgroup NuGet.Config ./

# Restore dependencies
RUN dotnet restore --verbosity minimal

# Copy source code
COPY --chown=appuser:appgroup . .

# Build the application
ARG BUILD_CONFIGURATION
RUN dotnet build \
    --configuration ${BUILD_CONFIGURATION} \
    --no-restore \
    --output /app/build

# Publish the application
RUN dotnet publish \
    --configuration ${BUILD_CONFIGURATION} \
    --no-build \
    --output /app/publish \
    /p:UseAppHost=false

# ============================================================================
# Stage 2: Test stage
# ============================================================================
FROM build AS test

# Run unit tests
RUN dotnet test \
    --configuration ${BUILD_CONFIGURATION} \
    --no-build \
    --verbosity normal \
    --logger "trx;LogFileName=test-results.trx" \
    --collect:"XPlat Code Coverage"

# ============================================================================
# Stage 3: Runtime stage
# ============================================================================
FROM mcr.microsoft.com/dotnet/aspnet:8.0-alpine AS runtime

# Security: Run as non-root
ENV ASPNETCORE_URLS=http://+:8080 \
    DOTNET_RUNNING_IN_CONTAINER=true \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=false

WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache \
    icu-libs \
    ca-certificates \
    tzdata \
    && update-ca-certificates

# Create non-root user
RUN addgroup -g 1000 appgroup \
    && adduser -u 1000 -G appgroup -D appuser

# Copy published application
COPY --from=build --chown=appuser:appgroup /app/publish .

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:8080/health || exit 1

# Default command
ENTRYPOINT ["dotnet", "UAST-Grep.dll"]
CMD ["--urls", "http://+:8080"]

# ============================================================================
# Stage 4: Development stage
# ============================================================================
FROM build AS development

# Install development tools
RUN dotnet tool install --global dotnet-watch \
    && dotnet tool install --global dotnet-ef

# Add tools to PATH
ENV PATH="${PATH}:/root/.dotnet/tools"

# Volume for source code
VOLUME ["/app/src"]

# Development command with hot reload
CMD ["dotnet", "watch", "run", "--project", "src/UAST-Grep.csproj"]

# ============================================================================
# Multi-platform example
# ============================================================================
# syntax=docker/dockerfile:1.4
FROM --platform=$BUILDPLATFORM golang:1.21-alpine AS go-build

ARG TARGETPLATFORM
ARG TARGETOS
ARG TARGETARCH

RUN --mount=type=cache,target=/go/pkg/mod \
    --mount=type=cache,target=/root/.cache/go-build \
    GOOS=$TARGETOS GOARCH=$TARGETARCH go build -o /app/binary .

# ============================================================================
# Conditional instructions example
# ============================================================================
FROM alpine:3.18 AS conditional

ARG INSTALL_DEV_DEPS=false

RUN if [ "$INSTALL_DEV_DEPS" = "true" ]; then \
        apk add --no-cache vim bash; \
    fi

# ADD vs COPY examples
ADD https://example.com/file.tar.gz /tmp/
COPY local-file.txt /app/

# ONBUILD for base images
ONBUILD COPY . /app
ONBUILD RUN npm install

# STOPSIGNAL
STOPSIGNAL SIGTERM

# SHELL instruction
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN echo "Using bash" | grep bash
