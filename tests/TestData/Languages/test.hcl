# HCL Test File for UAST-Grep
# Tests: blocks, attributes, expressions, functions, conditionals

# =============================================================================
# Variables
# =============================================================================

variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "UAST-Grep"
}

variable "environment" {
  description = "Deployment environment"
  type        = string
  default     = "development"

  validation {
    condition     = contains(["development", "staging", "production"], var.environment)
    error_message = "Environment must be development, staging, or production."
  }
}

variable "instance_count" {
  description = "Number of instances"
  type        = number
  default     = 2
}

variable "enable_monitoring" {
  description = "Enable monitoring"
  type        = bool
  default     = true
}

variable "tags" {
  description = "Resource tags"
  type        = map(string)
  default = {
    Project     = "UAST-Grep"
    Environment = "Development"
    ManagedBy   = "Terraform"
  }
}

variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)
  default     = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

variable "instance_config" {
  description = "Instance configuration"
  type = object({
    instance_type = string
    ami_id        = string
    volume_size   = number
    enable_ebs    = bool
  })
  default = {
    instance_type = "t3.medium"
    ami_id        = "ami-12345678"
    volume_size   = 50
    enable_ebs    = true
  }
}

# =============================================================================
# Locals
# =============================================================================

locals {
  # Simple values
  project_prefix = "${var.project_name}-${var.environment}"
  timestamp      = timestamp()

  # Computed values
  instance_name = "${local.project_prefix}-instance"

  # Conditional expression
  instance_size = var.environment == "production" ? "large" : "small"

  # Complex expressions
  common_tags = merge(var.tags, {
    Name        = local.project_prefix
    LastUpdated = local.timestamp
  })

  # For expressions
  az_names = [for az in var.availability_zones : upper(az)]

  az_map = { for idx, az in var.availability_zones : "zone-${idx}" => az }

  # Filtered list
  filtered_azs = [for az in var.availability_zones : az if !endswith(az, "c")]

  # Flattening
  all_subnets = flatten([
    for vpc in local.vpcs : [
      for subnet in vpc.subnets : {
        vpc_id    = vpc.id
        subnet_id = subnet.id
        cidr      = subnet.cidr
      }
    ]
  ])

  # Object example
  vpcs = [
    {
      id = "vpc-1"
      subnets = [
        { id = "subnet-1a", cidr = "10.0.1.0/24" },
        { id = "subnet-1b", cidr = "10.0.2.0/24" },
      ]
    },
    {
      id = "vpc-2"
      subnets = [
        { id = "subnet-2a", cidr = "10.1.1.0/24" },
      ]
    }
  ]
}

# =============================================================================
# Data Sources
# =============================================================================

data "aws_caller_identity" "current" {}

data "aws_region" "current" {}

data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-focal-20.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["099720109477"] # Canonical
}

data "aws_availability_zones" "available" {
  state = "available"

  filter {
    name   = "opt-in-status"
    values = ["opt-in-not-required"]
  }
}

# =============================================================================
# Resources
# =============================================================================

resource "aws_vpc" "main" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(local.common_tags, {
    Name = "${local.project_prefix}-vpc"
  })
}

resource "aws_subnet" "public" {
  count = length(var.availability_zones)

  vpc_id                  = aws_vpc.main.id
  cidr_block              = cidrsubnet(aws_vpc.main.cidr_block, 8, count.index)
  availability_zone       = var.availability_zones[count.index]
  map_public_ip_on_launch = true

  tags = merge(local.common_tags, {
    Name = "${local.project_prefix}-public-${count.index + 1}"
    Type = "Public"
  })
}

resource "aws_security_group" "web" {
  name        = "${local.project_prefix}-web-sg"
  description = "Security group for web servers"
  vpc_id      = aws_vpc.main.id

  dynamic "ingress" {
    for_each = [80, 443]
    content {
      from_port   = ingress.value
      to_port     = ingress.value
      protocol    = "tcp"
      cidr_blocks = ["0.0.0.0/0"]
    }
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = local.common_tags

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_instance" "web" {
  count = var.instance_count

  ami                    = data.aws_ami.ubuntu.id
  instance_type          = var.instance_config.instance_type
  subnet_id              = aws_subnet.public[count.index % length(aws_subnet.public)].id
  vpc_security_group_ids = [aws_security_group.web.id]

  root_block_device {
    volume_size = var.instance_config.volume_size
    volume_type = "gp3"
    encrypted   = true
  }

  user_data = <<-EOF
    #!/bin/bash
    echo "Hello from ${local.project_prefix}"
    apt-get update
    apt-get install -y nginx
  EOF

  tags = merge(local.common_tags, {
    Name  = "${local.instance_name}-${count.index + 1}"
    Index = count.index
  })

  depends_on = [aws_security_group.web]
}

# Conditional resource
resource "aws_cloudwatch_log_group" "app" {
  count = var.enable_monitoring ? 1 : 0

  name              = "/aws/${local.project_prefix}"
  retention_in_days = 30

  tags = local.common_tags
}

# For_each example
resource "aws_iam_user" "users" {
  for_each = toset(["alice", "bob", "charlie"])

  name = each.value
  path = "/users/"

  tags = merge(local.common_tags, {
    Username = each.value
  })
}

# =============================================================================
# Modules
# =============================================================================

module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "~> 5.0"

  name = local.project_prefix
  cidr = "10.0.0.0/16"

  azs             = var.availability_zones
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]

  enable_nat_gateway = var.environment == "production"
  single_nat_gateway = var.environment != "production"

  tags = local.common_tags
}

# =============================================================================
# Outputs
# =============================================================================

output "vpc_id" {
  description = "The ID of the VPC"
  value       = aws_vpc.main.id
}

output "subnet_ids" {
  description = "List of subnet IDs"
  value       = aws_subnet.public[*].id
}

output "instance_ips" {
  description = "Public IPs of instances"
  value       = aws_instance.web[*].public_ip
  sensitive   = false
}

output "instance_details" {
  description = "Instance details map"
  value = {
    for instance in aws_instance.web :
    instance.id => {
      public_ip  = instance.public_ip
      private_ip = instance.private_ip
      az         = instance.availability_zone
    }
  }
}

output "account_id" {
  description = "AWS Account ID"
  value       = data.aws_caller_identity.current.account_id
}

# =============================================================================
# Terraform Block
# =============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }

  backend "s3" {
    bucket         = "terraform-state-bucket"
    key            = "UAST-Grep/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
    dynamodb_table = "terraform-locks"
  }
}

provider "aws" {
  region = "us-east-1"

  default_tags {
    tags = local.common_tags
  }
}

provider "aws" {
  alias  = "west"
  region = "us-west-2"
}
