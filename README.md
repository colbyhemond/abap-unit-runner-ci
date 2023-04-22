# ABAP Unit Runner
This repository is meant for Continuous Integration pipelines.

It provides the capability to call into your SAP system in order to run the unit tests.

It can be triggered when abap code is merged into `main` or a pull request is opened, and allows all abap unit tests within the repository to be ran. 

# How it works
The script traverses the file directory of the repository looking for any `.clas.abap` files and then submits them to the endpoint `/abapunitrunner/ci/runner` where the class to be unit tested is submitted to program `rs_aucv_runner`.

# Benefits
* Tests all unit test classes of repository
* Can be automatically triggered
* Leverages SAP provided program to run the unit test
  * Testing is consistent with manually running unit tests in ADT or SAP GUI
  * Leverages Email template that includes ADT links for failed tests