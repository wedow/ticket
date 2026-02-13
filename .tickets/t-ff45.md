---
id: t-ff45
status: closed
deps: []
links: []
created: 2026-01-24T23:57:49Z
type: epic
priority: 2
assignee: Steve Macbeth
---
# Ticket Lifecycle Redesign

Overhaul ticket workflow to support hierarchy gating, new statuses, and auto-propagation

## Design

Adds workstream type, new status flow (open→in_progress→needs_testing→closed), readiness rules based on parent status and Plan section presence, automatic upward status propagation, and tk workflow command for LLM guidance

## Acceptance Criteria

All new statuses work, hierarchy gating works, auto-propagation works, tk workflow outputs guide

