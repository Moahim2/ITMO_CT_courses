package com.academy.fintech.origination.configuration;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

@Configuration
@EnableAsync
@EnableScheduling
@ConditionalOnProperty(prefix = "spring", name = "scheduler.enabled", matchIfMissing = true)
public class ScheduleConfiguration {}
