# Spec-Kit Structure

This directory follows the [GitHub Spec-Kit](https://github.com/github/spec-kit) framework for spec-driven development.

## Directory Structure

```
.specify/
├── README.md              # This file - overview of spec-kit structure
├── memory/                # Immutable project principles
│   └── constitution.md    # Project constitution and architectural guidelines
├── specs/                 # Feature specifications and plans
│   ├── spec.md           # High-level project specification
│   ├── plan.md           # Technical implementation plan
│   └── tasks.md          # Detailed task breakdown
└── templates/             # Templates for consistent documentation
```

## What is Spec-Kit?

Spec-Kit is a toolkit from GitHub that enables "spec-driven development" - a methodology where specifications (not code) are the primary artifact driving the software lifecycle. Instead of using prompts or chat logs as disposable aids, spec-kit elevates clear written specifications to first-class status.

## Files in This Repository

### constitution.md
Located in `.specify/memory/constitution.md`, this file establishes the "architectural DNA" for the project. It contains immutable principles that guide all development decisions, including:
- Legacy preservation requirements
- Testing mandates
- Architectural principles
- Educational priorities
- Quality gates

### spec.md
Located in `.specify/specs/spec.md`, this is the high-level project specification that describes:
- Project overview and background
- User stories and acceptance criteria
- Technical requirements
- System architecture
- Success criteria
- Constraints and dependencies

### plan.md
Located in `.specify/specs/plan.md`, this is the technical implementation plan that details:
- Implementation approach and strategy
- Technology stack and architecture
- Detailed implementation phases
- Technical decisions and rationale
- Risk management
- Success metrics

### tasks.md
Located in `.specify/specs/tasks.md`, this breaks down the plan into discrete, actionable tasks:
- Organized by implementation phase
- Each task has acceptance criteria
- Dependencies clearly identified
- Effort estimates provided
- Status tracking

## How to Use This Structure

1. **Start with Constitution**: Review `memory/constitution.md` to understand the project principles
2. **Read the Spec**: Review `specs/spec.md` to understand what needs to be built
3. **Review the Plan**: Read `specs/plan.md` to understand how it will be built
4. **Follow the Tasks**: Use `specs/tasks.md` to track implementation progress

## Benefits of Spec-Driven Development

- **Clarity**: Clear, written specifications reduce ambiguity
- **Alignment**: All stakeholders understand the goals and approach
- **Traceability**: Easy to track decisions and changes
- **AI-Friendly**: Provides context for AI coding assistants
- **Documentation**: Built-in documentation throughout development
- **Quality**: Constitutional principles ensure consistent quality

## This Project

This repository demonstrates the modernization of a COBOL account management system to Node.js. The spec-kit structure ensures that:
- Original COBOL code is preserved
- Functional equivalence is maintained
- Educational value is prioritized
- The process is well-documented
- Quality gates are enforced

For more information about the project, see the main [README.md](../README.md) in the repository root.

## References

- [GitHub Spec-Kit Repository](https://github.com/github/spec-kit)
- [Spec-Kit Documentation](https://speckit.org/)
- Project Specification: [specs/spec.md](specs/spec.md)
- Implementation Plan: [specs/plan.md](specs/plan.md)
- Task List: [specs/tasks.md](specs/tasks.md)
