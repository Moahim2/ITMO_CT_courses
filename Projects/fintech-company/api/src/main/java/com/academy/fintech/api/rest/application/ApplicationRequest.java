package com.academy.fintech.api.rest.application;

import com.fasterxml.jackson.annotation.JsonProperty;

public record ApplicationRequest(
        @JsonProperty("first_name")
        String firstName,
        @JsonProperty("last_name")
        String lastName,
        String email,
        int salary,
        int amount
) { }
