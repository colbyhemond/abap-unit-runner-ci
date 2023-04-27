# ABAP Unit Runner
This repository is meant for Continuous Integration pipelines, with your common shared development environment in mind.

It provides the capability to call into your SAP system in order to run the unit tests that have been developed in there.

It can be triggered when abap code is merged into `main` or a pull request is opened, and allows all abap unit tests within the repository to be ran.

This is a decent starting point for implementing CI into your deployment workflow. If your team is too big, or your packages are too big, you could possibly start to see where further organization or isolated development systems could help.

# Set Up
* clone into your SAP system via abapGit
* activate ICF Nodes `/abapunitrunner/ci/runner` via transaction code `SICF`
* Set up pipeline task
* Add environment variables to your pipeline:
  * `SAP_USERNAME`
  * `SAP_PASSWORD`
  * `SAP_DEFAULT_HOST`
  * **MAKE SURE THESE DETAILS ARE SECURE! MAKE SURE YOU DO NOT PUSH THESE VALUES TO A PUBLIC (OR PRIVATE) REPOSITORY!**


## Pipeline Task for Azure Dev Ops
[Create your first pipeline with with Azure pipelines](https://learn.microsoft.com/en-us/azure/devops/pipelines/create-first-pipeline?view=azure-devops&tabs=javascript%2Ctfs-2018-2%2Cbrowser)
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
  
# Drawbacks
* If too many developers use the same dev sytem, and mutliple developers are creating unit tests in the same package, someones temporary failing test might affectwhen the CI step runs all the tests
  * You can create morne granular packages and work on managing coupling in your application to possible help this
  * If this can't be avoided, then you are in the same kind of bottle neck as "locked objects with transports" and suggest re-examining your CI plan. (add isolated system, unit test with transpoiled abap, etc..)

# Comparisons
This solution is not better than other out there, it simply helps fill a gap giving you another option to define your development/deployment workflow how you want it to work.

## Other Options
* Set up ATC rule to automatically run unit tests when a transport is released
* Manually Run Unit Test on your class or package via SAP GUI or ADT in Eclipse
* Run unit test with open-abap, where your test classes are transpiled to JavaScript and ran on a temporary node server

All of these solutions have their place depending on when you want unit tests to be ran, and your development/deployment workflows

## Additional Resources on CI/CD with ABAP
* [SAP Press Blog - ABAP and Continuous Integration: the ABAP Test Cockpit and the ABAP Unit Runner](https://blog.sap-press.com/abap-and-continuous-integration-the-abap-test-cockpit-and-the-abap-unit-runner)
* [Lars Hvam - So, what is CI?](https://blogs.sap.com/2020/12/28/so-what-is-ci/)
* [Lars Hvam - Fully supporting CI in ABAP AS](https://blogs.sap.com/2020/02/20/fully-supporting-ci-in-abap-as/)

