*Not Complete - Work in Progress - Open to help and ideas*

# ABAP Unit Runner
This repository is meant for Continuous Integration pipelines, with your common shared development environment in mind.

It provides the capability to call into your SAP system in order to run the unit tests.

It can be triggered when abap code is merged into `main` or a pull request is opened, and allows all abap unit tests within the repository to be ran. 

# Set Up
* clone into your SAP system via abapGit
* activate ICF Nodes `/abapunitrunner/ci/runner` via transaction code `SICF`
* Set up pipeline task

## Pipeline Task for Azure Dev Ops
``` yaml
steps:
- script: |
    echo installing abap-unit-runner-cli
    npm install https://github.com/colbyhemond/abap-unit-runner-ci.git
    echo running abap unit
    npm run abap-unit
  displayName: 'ABAP Unit Tests'
```

# How it works
The script traverses the file directory of the repository looking for any `.clas.abap` files and then submits them to the endpoint `/abapunitrunner/ci/runner` where the class to be unit tested is submitted to program `rs_aucv_runner`.

Program `rs_aucv_runner` is a standard SAP program to run unit tests, therefore leveraging the same framework as ADT and the SAP GUI does when manually running unit tests for a class. The program is submitted as a background job and can be found in transaction `SM37` looking for job name `/CI/ABAP_UNIT_RUNNER`.

If a unit test fails, an email will be sent to the user. Check transaction `SOST` for emails with a subject line like "[1] :-( ABAP Unit Runner S4H".

# Benefits
* Tests all unit test classes of repository
* Can be automatically triggered from Git Repositories
* Automation is decoupled from transports
* Leverages standard SAP provided program to run the unit tests:
  * Tests use same framework as when manually running unit tests in ADT or SAP GUI
  * Leverages Email template that includes ADT links for failed tests for quicker navigation

