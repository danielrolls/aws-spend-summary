# AWS Spend Summary Tool

This tool prints your recent, daily AWS cost usage to the command line to save logging in to the console to check it.

Credentials are read from `~/.aws/credentials` and account details from 
`~/.aws/config`. These can be setup by installing and running `awscli`. It is recommended to use IAM to limit access to just the Cost Explorer Service. You can use a policy like this:

```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "VisualEditor0",
            "Effect": "Allow",
            "Action": [
                "ce:GetCostAndUsage",
                "ce:GetReservationPurchaseRecommendation",
                "ce:GetPreferences",
                "ce:ListSavingsPlansPurchaseRecommendationGeneration",
                "ce:ListTagsForResource",
                "ce:GetReservationUtilization",
                "ce:GetCostCategories",
                "ce:GetSavingsPlansPurchaseRecommendation",
                "ce:GetSavingsPlansUtilizationDetails",
                "ce:GetDimensionValues",
                "ce:GetAnomalySubscriptions",
                "ce:DescribeReport",
                "ce:GetReservationCoverage",
                "ce:GetAnomalyMonitors",
                "ce:GetUsageForecast",
                "ce:DescribeNotificationSubscription",
                "ce:DescribeCostCategoryDefinition",
                "ce:GetRightsizingRecommendation",
                "ce:GetSavingsPlansUtilization",
                "ce:GetAnomalies",
                "ce:ListCostCategoryDefinitions",
                "ce:GetCostForecast",
                "ce:GetApproximateUsageRecords",
                "ce:GetCostAndUsageWithResources",
                "ce:ListCostAllocationTags",
                "ce:GetSavingsPlanPurchaseRecommendationDetails",
                "ce:GetSavingsPlansCoverage",
                "ce:GetConsoleActionSetEnforced",
                "ce:GetTags",
                "ce:ListCostAllocationTagBackfillHistory"
            ],
            "Resource": "*"
        }
    ]
}
```
