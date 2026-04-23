# Proposal: Agent-Agnostic Skill Document Initiative for Biostatistics Workflows

**Prepared by:** Junhan Fang  
**Date:** April 2026  
**Status:** Draft for Review

---

## 1. Background and Motivation

AI agents (Claude, ChatGPT, Gemini, and others) are increasingly available as productivity tools in clinical trial workflows. However, these agents have no built-in knowledge of our specific trial design methods, validation standards, or internal software stack. Without structured guidance, outputs from different agents are inconsistent and difficult to audit.

The **skill document initiative** addresses this by formalizing each biostatistics procedure as an agent-agnostic `.md` skill file. Each skill document defines:

- **Purpose** — what the calculation achieves and which R package powers it
- **Inputs** — all required parameters, types, defaults, and constraints
- **Procedure** — complete, ready-to-run R code
- **Output** — exact fields, column names, and rounding rules
- **Validation rules** — boundary conditions and known gotchas
- **Verified example** — a locked input/output pair for regression testing

### Why this matters

| Benefit | Detail |
|---------|--------|
| **Tool-agnostic** | Any AI agent, R console, or human analyst can follow the same procedure |
| **Reproducibility** | Results are traceable back to a versioned, reviewable document |
| **Reduced onboarding time** | New team members or agents can execute validated workflows without tribal knowledge |
| **Regulatory defensibility** | Skill docs serve as a written SOP equivalent for AI-assisted calculations |
| **Reusability** | One skill document covers all trials using the same methodology |

---

## 2. Scope

The initiative covers biostatistics calculation skills organized into two delivery stages, differentiated by their dependency on external clinical data systems.

### Stage 1 — Standalone Calculations (No Database Required)

These skills perform prospective design calculations using only analyst-supplied parameters. They can run entirely in R without any connection to a clinical database.

| Skill | Method | Package |
|-------|--------|---------|
| Sample Size — Proportional Hazards | Group sequential, rpact | `rpact` |
| Sample Size — Non-Proportional Hazards | Delayed treatment effect, gsDesign2 | `gsDesign2` |
| IA Boundary Re-calculation | Alpha/beta spending, z-scale + HR-scale | `rpact` |
| Event Projection | Projected event counts over time | TBD |

### Stage 2 — Database-Linked Analysis Validation

These skills validate observed trial results against a live or frozen dataset. They require the agent to read subject-level data from an external system (entimice, LSAF, or equivalent SAS/ADaM environment).

| Skill | Method | Data Dependency |
|-------|--------|-----------------|
| Time-to-Event Analysis: OS | Kaplan-Meier, log-rank, Cox PH | ADaM ADTTE (entimice / LSAF) |
| Time-to-Event Analysis: PFS | Kaplan-Meier, log-rank, Cox PH | ADaM ADTTE (entimice / LSAF) |

Stage 2 introduces additional complexity: data access authentication, field mapping across studies, and validation against independently-derived reference results. This warrants a separate planning phase.

---

## 3. Proposed Tasks and Timeline

### Task 1 — Skill Inventory and Prioritization

**Objective:** Confirm the complete list of required skill documents, finalize the implementation order, and align on scope boundaries with key stakeholders.

**Deliverables:**
- Approved skill list with priority ranking
- Defined scope for Stage 1 vs Stage 2
- Decision on which database systems to support in Stage 2 (entimice, LSAF, or both)

**Proposed order of implementation:**
1. Sample Size — Proportional Hazards (PH)
2. Sample Size — Non-Proportional Hazards (NPH)
3. IA Boundary Re-calculation
4. Event Projection
5. Time-to-Event Analysis (OS / PFS)

**Rationale for order:** PH and NPH are the most frequently requested calculations and serve as the foundation. IA Boundary is closely related and reuses the same rpact infrastructure. Event Projection extends the timeline framework. TTE analysis validation is deferred to Stage 2 due to its database dependency.

| Activity | Duration |
|----------|----------|
| Stakeholder alignment meeting | Week 1 |
| Finalize skill list and order | Week 1–2 |
| Document scope decisions | Week 2 |

**Estimated duration: 2 weeks**

---

### Task 2 — Skill Document Development

#### Stage 1 — Standalone Calculation Skills

Each skill document follows a standard template and is developed, tested against a verified example, and peer-reviewed before release.

| Skill | Development | Internal Review | Estimated Completion |
|-------|-------------|-----------------|----------------------|
| PH Sample Size | Week 3 | Week 4 | End of Week 4 |
| NPH Sample Size | Week 5 | Week 6 | End of Week 6 |
| IA Boundary | Week 7 | Week 8 | End of Week 8 |
| Event Projection | Week 9–10 | Week 11 | End of Week 11 |

> Note: PH, NPH, and IA Boundary skill documents have been drafted as part of a proof-of-concept and are available for review on the `skills-docs` branch of the StudyDesign repository. These will be refined during the formal development phase.

**Stage 1 estimated duration: ~9 weeks** (Weeks 3–11)

#### Stage 2 — Database-Linked Analysis Skills

Stage 2 requires additional groundwork before skill documents can be written: agreeing on the database access model, field mapping standards, and validation methodology.

| Activity | Duration |
|----------|----------|
| Database access design (entimice / LSAF integration) | Weeks 12–13 |
| Field mapping: ADaM ADTTE → skill inputs | Week 14 |
| OS analysis skill — development + verification | Weeks 15–16 |
| PFS analysis skill — development + verification | Weeks 17–18 |
| End-to-end validation with real trial data | Weeks 19–20 |
| Documentation and sign-off | Week 21 |

**Stage 2 estimated duration: ~10 weeks** (Weeks 12–21)

---

## 4. Summary Timeline

```
Week  1–2:   Task 1 — Inventory and prioritization
Week  3–4:   PH Sample Size skill
Week  5–6:   NPH Sample Size skill
Week  7–8:   IA Boundary skill
Week  9–11:  Event Projection skill
Week 12–13:  Stage 2 planning — database integration design
Week 14:     Field mapping (ADaM → skill inputs)
Week 15–16:  OS analysis skill
Week 17–18:  PFS analysis skill
Week 19–20:  End-to-end validation
Week 21:     Final review and sign-off
```

**Total estimated duration: ~21 weeks (~5 months)**

---

## 5. Resource Requirements

| Resource | Need |
|----------|------|
| Biostatistician (author) | Primary developer of all skill documents |
| Statistical reviewer | Peer review of verified examples and validation rules |
| IT / Data Engineering (Stage 2 only) | Database access configuration for entimice / LSAF |
| Stakeholder sign-off | Skill list approval (Task 1) and Stage 2 scope confirmation |

---

## 6. Next Steps

1. Schedule a Task 1 alignment meeting to confirm the skill list and order
2. Review the existing proof-of-concept skill documents (`skills/` directory, `skills-docs` branch)
3. Agree on the Stage 2 database systems in scope (entimice, LSAF, or both)
4. Confirm reviewer availability before development begins in Week 3

---

*For questions or feedback, please contact Junhan Fang.*
