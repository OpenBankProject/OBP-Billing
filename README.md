# OBP-Billing

A solution for the Billing / Monetisation of API usage 

API Monetisation in OBP is realised using five components: 

(1) The OBP API logs, stores and provides APIs for API usage (API Metrics). Also OBP provides APIs to read and set Consumer Rate Limiting. (2) The OBP Billing App generates Clients and Invoices in the (3) REST based Accounting system (e.g. Invoice Ninja) based on the API usage of each Consumer and can decide to limit the Consumer via the OBP Rate Limiting APIs if bills are not settled. The accounting system sends invoices to the (4) TPP Application (API Consumer) for the usage of the OBP API. API Consumer makes the payment using the (5) Payment system (e.g. Stripe) which is connected to the Invoice system and which sends information back to the Accounting system about records of payments. Periodically, the OBP Billing App queries the Accounting system API and blocks API overdue Consumers via the OBP Rate Limiting and Consumer APIs.

(OBP Monetisation, Billing and Rate Limiting)[https://user-images.githubusercontent.com/485218/74542852-ac946c00-4f44-11ea-9ac7-58a2b1453fa4.png]
