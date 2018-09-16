--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.10
-- Dumped by pg_dump version 10.5 (Ubuntu 10.5-0ubuntu0.18.04)

-- Started on 2018-09-10 19:37:33 -03

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 8 (class 2615 OID 16385)
-- Name: contacts_app; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA contacts_app;


ALTER SCHEMA contacts_app OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 186 (class 1259 OID 16386)
-- Name: contacts; Type: TABLE; Schema: contacts_app; Owner: postgres
--

CREATE TABLE contacts_app.contacts (
    email text NOT NULL,
    name text NOT NULL,
    surname text NOT NULL,
    phone_number text NOT NULL,
    active boolean NOT NULL
);


ALTER TABLE contacts_app.contacts OWNER TO postgres;

--
-- TOC entry 2003 (class 2606 OID 16393)
-- Name: contacts contacts_pkey; Type: CONSTRAINT; Schema: contacts_app; Owner: postgres
--

ALTER TABLE ONLY contacts_app.contacts
    ADD CONSTRAINT contacts_pkey PRIMARY KEY (email);


-- Completed on 2018-09-10 19:37:33 -03

--
-- PostgreSQL database dump complete
--

