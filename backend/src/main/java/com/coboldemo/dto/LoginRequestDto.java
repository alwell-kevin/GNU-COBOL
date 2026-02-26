package com.coboldemo.dto;

import java.util.Objects;

/**
 * Mirrors the legacy login prompt payload (username/password) that Main menu uses.
 */
public class LoginRequestDto {
  private String username;
  private String password;

  public LoginRequestDto() {}

  public LoginRequestDto(String username, String password) {
    this.username = Objects.requireNonNull(username);
    this.password = Objects.requireNonNull(password);
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }
}
