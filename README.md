contacts_crud
=====

A Cowboy REST application that exposes a Postgres CRUD API

Introduction
=====

This in an example of an application built on top of Cowboy Erlang HTTP server that uses Epgsql Postgres driver to persist it's data and exposes an API for contacts resource.

Cowboy is the most used HTTP server for Erlang/OTP environments, it has one of the most active Github repositories in Erlang community.

Epgsql Postgres database driver seemed like a simple way to persist data in a relational database that runs on Erlang 19+ (unlike Emysql at the moment),  without having to learn to use more complex frameworks like sumo_db.

Leader_cron it's used to remove contacts set as not active every 10 minutes.

It's only suitable for experimental/educational purposes.

Contacts
======

The postgres database schema used for this project is extremely simple.

Schema is named contact_app and there's only one table named contacts with five fields:

|Field name|Constraint|Type|
|---|---|---|
|email| private_key, NOT NULL| text|
|name|NOT NULL| text|
|surname|NOT NULL| text|
|phone_number|NOT NULL| text|
|active|NOT NULL| boolean|

API
=====



|  URL Path|  Method | Payload  | Status Code  |  Response Body |
|---|---|---|---|---|---|
| /contacts  |  GET |   | 200  |   List of users ordered by surname|
| /contacts | POST  | JSON with: email, name, surname, phone_number as keys |  204, 400 if payload is malformed or 409 if email already exists |
| /contacts/:email | PUT |  JSON with: name, surname, phone_number as keys | 204, 404 if resource is not found, 400 if payload is malformed |  
|/contacts/surname/:surname | GET | | 200, 404 if resource is not found | JSON of one user with matching surname|
|/contacts/delete/:email|DELETE| | 204, 404 if resource is not found 

**Note**: DELETE /contacts/delete/:email doesn't actually deletes the resource. It marks it as non active and there's a croned task that removes them every 10 minutes.

Example curls
---

    curl --header "Content-Type: application/json" \
    --request GET \
    http://localhost:8080/contacts

    curl --header "Content-Type: application/json" \
    --request POST \
    --data '{"name":"examplename","surname":"examplesurname", "phone_number":"+541231231", "email":"user@example.com" }' \
    http://localhost:8080/contacts

    curl --header "Content-Type: application/json" \
    --request PUT \
    --data '{"name":"examplename2","surname":"example_surname2", "phone_number":"+541111111" }' \
    http://localhost:8080/contacts/user@example.com

    curl --header "Content-Type: application/json" \
    --request GET \
    http://localhost:8080/contacts/surname/examplesurname

    curl --header "Content-Type: application/json"  --request DELETE   http://localhost:8080/contacts/delete/user@example.com

Build and run docker image
=====
Dockerfile compiles project as release with prod configuration.
To run that image container along with an already configured postgres service you can run:

    docker-compose build
    docker-compose -f docker-compose.yml up

Tests and formatting
====

To setup a postgres service container to run common tests run:

    docker-compose -f docker-compose-test.yml up

Then:

    rebar3 ct
    rebar3 eunit

Coverage
----

  |                      module  |  coverage  |
  |------------------------------|------------|
  |       contacts_crud_queries  |      100%  |
  |        contacts_crud_errors  |      100%  |
  |  contacts_crud_http_handler  |       90%  |
  |           contacts_crud_sup  |      100%  |
  |      contacts_crud_conn_mgr  |       63%  |
  |         contacts_crud_types  |      100%  |
  |           contacts_crud_app  |       85%  |
  |                       total  |       87%  |

Formatting
-----
    rebar3 dialyzer
    rebar3 as lint lint

Both don't return warnings

  Missing Features
  =====

  * Make release take postgres configuration values from OS environment variables and not from config file
  * Reduce docker image size using erlang-21 as builder
  * Metrics and functionality monitoring
  * API Payload validation


