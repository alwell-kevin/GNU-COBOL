package com.coboldemo.config;

import java.io.IOException;
import java.time.Clock;
import java.time.Instant;
import java.util.UUID;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.MDC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.web.filter.OncePerRequestFilter;

@Configuration
public class LoggingConfig {
  private static final Logger log = LoggerFactory.getLogger(LoggingConfig.class);

  @Bean
  public FilterRegistrationBean<TransactionLoggingFilter> transactionLoggingFilter(Clock clock) {
    FilterRegistrationBean<TransactionLoggingFilter> registration = new FilterRegistrationBean<>();
    registration.setFilter(new TransactionLoggingFilter(clock));
    registration.setOrder(Ordered.HIGHEST_PRECEDENCE);
    registration.addUrlPatterns("/api/*");
    return registration;
  }

  static final class TransactionLoggingFilter extends OncePerRequestFilter {
    private static final Logger filterLog = LoggerFactory.getLogger(TransactionLoggingFilter.class);
    private final Clock clock;

    TransactionLoggingFilter(Clock clock) {
      this.clock = clock;
    }

    @Override
    protected void doFilterInternal(
        HttpServletRequest request,
        HttpServletResponse response,
        FilterChain filterChain)
        throws ServletException, IOException {
      String transactionId = UUID.randomUUID().toString();
      String timestamp = Instant.now(clock).toString();
      MDC.put("transactionId", transactionId);
      MDC.put("transactionTimestamp", timestamp);
      filterLog.debug("Starting request {} {} [{}]", request.getMethod(), request.getRequestURI(), transactionId);
      try {
        filterChain.doFilter(request, response);
      } finally {
        MDC.remove("transactionId");
        MDC.remove("transactionTimestamp");
        filterLog.debug("Finished request {} {} [{}]", request.getMethod(), request.getRequestURI(), transactionId);
      }
    }
  }
}
