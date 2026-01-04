// Bicep Test File for UAST-Grep
// Tests: resources, parameters, variables, modules, outputs

// Target scope
targetScope = 'resourceGroup'

// =============================================================================
// Parameters
// =============================================================================

@description('Name of the project')
@minLength(3)
@maxLength(24)
param projectName string = 'uastgrep'

@description('Environment name')
@allowed([
  'dev'
  'staging'
  'prod'
])
param environment string = 'dev'

@description('Location for all resources')
param location string = resourceGroup().location

@description('Enable public access')
param enablePublicAccess bool = true

@description('Number of instances')
@minValue(1)
@maxValue(10)
param instanceCount int = 2

@description('Tags for all resources')
param tags object = {
  Project: 'UAST-Grep'
  Environment: environment
  ManagedBy: 'Bicep'
}

@description('Administrator password')
@secure()
param adminPassword string

@description('Allowed IP addresses')
param allowedIpAddresses array = [
  '10.0.0.0/8'
  '172.16.0.0/12'
]

// =============================================================================
// Variables
// =============================================================================

// Simple variables
var resourcePrefix = '${projectName}-${environment}'
var storageAccountName = '${replace(projectName, '-', '')}${uniqueString(resourceGroup().id)}'
var appServicePlanName = '${resourcePrefix}-asp'
var webAppName = '${resourcePrefix}-app'
var sqlServerName = '${resourcePrefix}-sql'
var keyVaultName = '${resourcePrefix}-kv'

// Conditional variable
var skuName = environment == 'prod' ? 'P1v3' : 'B1'
var skuTier = environment == 'prod' ? 'PremiumV3' : 'Basic'

// Object variable
var defaultTags = union(tags, {
  CreatedDate: utcNow('yyyy-MM-dd')
  TemplateVersion: '1.0.0'
})

// Array variable
var subnetConfigs = [
  {
    name: 'web-subnet'
    addressPrefix: '10.0.1.0/24'
    serviceEndpoints: ['Microsoft.Web', 'Microsoft.Sql']
  }
  {
    name: 'db-subnet'
    addressPrefix: '10.0.2.0/24'
    serviceEndpoints: ['Microsoft.Sql']
  }
  {
    name: 'app-subnet'
    addressPrefix: '10.0.3.0/24'
    serviceEndpoints: ['Microsoft.Web']
  }
]

// =============================================================================
// Resources
// =============================================================================

// Storage Account
resource storageAccount 'Microsoft.Storage/storageAccounts@2023-01-01' = {
  name: storageAccountName
  location: location
  tags: defaultTags
  kind: 'StorageV2'
  sku: {
    name: 'Standard_LRS'
  }
  properties: {
    accessTier: 'Hot'
    supportsHttpsTrafficOnly: true
    minimumTlsVersion: 'TLS1_2'
    allowBlobPublicAccess: false
    networkAcls: {
      bypass: 'AzureServices'
      defaultAction: enablePublicAccess ? 'Allow' : 'Deny'
      ipRules: [for ip in allowedIpAddresses: {
        value: ip
        action: 'Allow'
      }]
    }
  }

  // Nested resource
  resource blobServices 'blobServices' = {
    name: 'default'

    resource container 'containers' = {
      name: 'data'
      properties: {
        publicAccess: 'None'
      }
    }
  }
}

// App Service Plan
resource appServicePlan 'Microsoft.Web/serverfarms@2023-01-01' = {
  name: appServicePlanName
  location: location
  tags: defaultTags
  kind: 'linux'
  sku: {
    name: skuName
    tier: skuTier
  }
  properties: {
    reserved: true
  }
}

// Web App with loop
resource webApps 'Microsoft.Web/sites@2023-01-01' = [for i in range(0, instanceCount): {
  name: '${webAppName}-${i}'
  location: location
  tags: union(defaultTags, {
    Instance: string(i)
  })
  kind: 'app,linux'
  properties: {
    serverFarmId: appServicePlan.id
    httpsOnly: true
    siteConfig: {
      linuxFxVersion: 'DOTNETCORE|8.0'
      alwaysOn: environment == 'prod'
      ftpsState: 'Disabled'
      minTlsVersion: '1.2'
      appSettings: [
        {
          name: 'WEBSITE_RUN_FROM_PACKAGE'
          value: '1'
        }
        {
          name: 'APPINSIGHTS_INSTRUMENTATIONKEY'
          value: appInsights.properties.InstrumentationKey
        }
        {
          name: 'StorageConnection'
          value: '@Microsoft.KeyVault(SecretUri=${keyVault::storageSecret.properties.secretUri})'
        }
      ]
    }
  }
  identity: {
    type: 'SystemAssigned'
  }
}]

// SQL Server
resource sqlServer 'Microsoft.Sql/servers@2023-02-01-preview' = {
  name: sqlServerName
  location: location
  tags: defaultTags
  properties: {
    administratorLogin: 'sqladmin'
    administratorLoginPassword: adminPassword
    version: '12.0'
    minimalTlsVersion: '1.2'
    publicNetworkAccess: enablePublicAccess ? 'Enabled' : 'Disabled'
  }
}

// SQL Database
resource sqlDatabase 'Microsoft.Sql/servers/databases@2023-02-01-preview' = {
  parent: sqlServer
  name: '${projectName}db'
  location: location
  tags: defaultTags
  sku: {
    name: environment == 'prod' ? 'S1' : 'Basic'
    tier: environment == 'prod' ? 'Standard' : 'Basic'
  }
  properties: {
    collation: 'SQL_Latin1_General_CP1_CI_AS'
    maxSizeBytes: 2147483648
    zoneRedundant: environment == 'prod'
  }
}

// Key Vault
resource keyVault 'Microsoft.KeyVault/vaults@2023-02-01' = {
  name: keyVaultName
  location: location
  tags: defaultTags
  properties: {
    tenantId: subscription().tenantId
    sku: {
      family: 'A'
      name: 'standard'
    }
    enableRbacAuthorization: true
    enableSoftDelete: true
    softDeleteRetentionInDays: 90
    networkAcls: {
      bypass: 'AzureServices'
      defaultAction: 'Deny'
    }
  }

  // Secret
  resource storageSecret 'secrets' = {
    name: 'StorageConnectionString'
    properties: {
      value: 'DefaultEndpointsProtocol=https;AccountName=${storageAccount.name};AccountKey=${storageAccount.listKeys().keys[0].value}'
    }
  }
}

// Application Insights
resource appInsights 'Microsoft.Insights/components@2020-02-02' = {
  name: '${resourcePrefix}-ai'
  location: location
  tags: defaultTags
  kind: 'web'
  properties: {
    Application_Type: 'web'
    Request_Source: 'rest'
    RetentionInDays: 90
    publicNetworkAccessForIngestion: 'Enabled'
    publicNetworkAccessForQuery: 'Enabled'
  }
}

// Virtual Network
resource virtualNetwork 'Microsoft.Network/virtualNetworks@2023-05-01' = {
  name: '${resourcePrefix}-vnet'
  location: location
  tags: defaultTags
  properties: {
    addressSpace: {
      addressPrefixes: ['10.0.0.0/16']
    }
    subnets: [for subnet in subnetConfigs: {
      name: subnet.name
      properties: {
        addressPrefix: subnet.addressPrefix
        serviceEndpoints: [for endpoint in subnet.serviceEndpoints: {
          service: endpoint
        }]
      }
    }]
  }
}

// Role Assignment
resource keyVaultRoleAssignment 'Microsoft.Authorization/roleAssignments@2022-04-01' = [for i in range(0, instanceCount): {
  name: guid(keyVault.id, webApps[i].id, 'Key Vault Secrets User')
  scope: keyVault
  properties: {
    roleDefinitionId: subscriptionResourceId('Microsoft.Authorization/roleDefinitions', '4633458b-17de-408a-b874-0445c86b69e6')
    principalId: webApps[i].identity.principalId
    principalType: 'ServicePrincipal'
  }
}]

// =============================================================================
// Modules
// =============================================================================

// Module reference
module monitoring 'modules/monitoring.bicep' = {
  name: 'monitoring-deployment'
  params: {
    projectName: projectName
    environment: environment
    location: location
    tags: defaultTags
    appInsightsId: appInsights.id
  }
}

// Conditional module
module backup 'modules/backup.bicep' = if (environment == 'prod') {
  name: 'backup-deployment'
  params: {
    storageAccountId: storageAccount.id
    sqlDatabaseId: sqlDatabase.id
  }
}

// =============================================================================
// Outputs
// =============================================================================

@description('Storage account name')
output storageAccountName string = storageAccount.name

@description('Storage account blob endpoint')
output storageEndpoint string = storageAccount.properties.primaryEndpoints.blob

@description('Web app URLs')
output webAppUrls array = [for i in range(0, instanceCount): 'https://${webApps[i].properties.defaultHostName}']

@description('SQL Server FQDN')
output sqlServerFqdn string = sqlServer.properties.fullyQualifiedDomainName

@description('Key Vault URI')
output keyVaultUri string = keyVault.properties.vaultUri

@description('Application Insights instrumentation key')
@secure()
output appInsightsKey string = appInsights.properties.InstrumentationKey

@description('Resource IDs')
output resourceIds object = {
  storageAccount: storageAccount.id
  appServicePlan: appServicePlan.id
  keyVault: keyVault.id
  virtualNetwork: virtualNetwork.id
}
