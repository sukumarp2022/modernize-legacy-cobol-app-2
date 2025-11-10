# Implementation Plan: COBOL to Node.js Modernization

## Approach

This plan follows a **modular, incremental migration** strategy, converting the COBOL account management system to Node.js while maintaining functional equivalence. The approach prioritizes:

1. **Preservation**: Keep original COBOL code intact
2. **Equivalence**: Ensure Node.js version replicates all COBOL behavior
3. **Testing**: Validate each module against documented test cases
4. **Education**: Document the process for learning purposes

## Architecture

### Module Structure

The Node.js application will mirror the three-module COBOL architecture:

```
COBOL Structure          →    Node.js Structure
─────────────────────────────────────────────────
main.cob                 →    main.js
  (UI & Menu Logic)           (Console Interface)
                               
operations.cob           →    operations.js
  (Business Logic)            (Account Operations)
                               
data.cob                 →    data.js
  (Data Layer)                (Balance Storage)
```

### Technology Stack

**Core Technologies**:
- **Runtime**: Node.js (v14+)
- **Language**: JavaScript (ES6+)
- **Console I/O**: readline module (Node.js built-in)
- **Testing**: Jest or Mocha (to be determined during implementation)

**Development Tools**:
- **IDE**: Visual Studio Code with GitHub Copilot
- **Version Control**: Git
- **Package Manager**: npm

### Design Principles

Following the project constitution:

1. **Modular Design**: Separate concerns across three distinct modules
2. **Minimal Dependencies**: Use Node.js built-in modules where possible
3. **Clear Interfaces**: Define explicit module contracts matching COBOL CALL patterns
4. **Synchronous Flow**: Maintain synchronous execution model similar to COBOL
5. **Console-Based**: Interactive terminal interface using readline

## Implementation Phases

### Phase 1: Project Setup and Planning ✓

**Status**: Complete

**Activities**:
- ✅ Analyze existing COBOL code structure
- ✅ Document current test plan (TESTPLAN.md exists)
- ✅ Create spec-kit structure (.specify directory)
- ✅ Define constitution, spec, and implementation plan
- ✅ Set up project documentation

### Phase 2: Data Layer Migration

**Objective**: Convert data.cob to data.js

**Current COBOL Implementation** (data.cob):
- Stores account balance in working storage
- Provides READ and WRITE operations
- Uses LINKAGE section for parameter passing
- Initial balance: 1000.00

**Node.js Implementation** (data.js):
```javascript
// Module pattern for data encapsulation
let accountBalance = 1000.00;

function readBalance() {
    return accountBalance;
}

function writeBalance(newBalance) {
    accountBalance = newBalance;
    return accountBalance;
}

module.exports = {
    readBalance,
    writeBalance
};
```

**Technical Decisions**:
- Use module-level variable for balance storage (equivalent to COBOL WORKING-STORAGE)
- Export functions for read/write operations (equivalent to CALL interface)
- Maintain state across calls within same session
- Use floating-point numbers with .toFixed(2) for currency formatting

**Validation**:
- Create unit tests for readBalance and writeBalance functions
- Verify initial balance is 1000.00
- Test multiple read/write cycles
- Confirm state persistence across operations

### Phase 3: Operations Layer Migration

**Objective**: Convert operations.cob to operations.js

**Current COBOL Implementation** (operations.cob):
- Accepts operation type parameter: 'TOTAL', 'CREDIT', 'DEBIT'
- Calls data.cob for balance read/write
- Handles user input for credit/debit amounts
- Implements overdraft protection for debits
- Displays results to user

**Node.js Implementation** (operations.js):
```javascript
const readline = require('readline');
const data = require('./data');

// Create readline interface for user input
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

async function handleOperation(operationType) {
    switch(operationType) {
        case 'TOTAL':
            await viewBalance();
            break;
        case 'CREDIT':
            await creditAccount();
            break;
        case 'DEBIT':
            await debitAccount();
            break;
        default:
            console.log('Invalid operation type');
    }
}

async function viewBalance() {
    const balance = data.readBalance();
    console.log(`Current balance: ${balance.toFixed(2)}`);
}

async function creditAccount() {
    return new Promise((resolve) => {
        rl.question('Enter credit amount: ', (input) => {
            const amount = parseFloat(input);
            if (isNaN(amount) || amount < 0) {
                console.log('Invalid amount');
                resolve();
                return;
            }
            const currentBalance = data.readBalance();
            const newBalance = currentBalance + amount;
            data.writeBalance(newBalance);
            console.log(`Amount credited. New balance: ${newBalance.toFixed(2)}`);
            resolve();
        });
    });
}

async function debitAccount() {
    return new Promise((resolve) => {
        rl.question('Enter debit amount: ', (input) => {
            const amount = parseFloat(input);
            if (isNaN(amount) || amount < 0) {
                console.log('Invalid amount');
                resolve();
                return;
            }
            const currentBalance = data.readBalance();
            if (amount > currentBalance) {
                console.log('Insufficient funds for this debit.');
                resolve();
                return;
            }
            const newBalance = currentBalance - amount;
            data.writeBalance(newBalance);
            console.log(`Amount debited. New balance: ${newBalance.toFixed(2)}`);
            resolve();
        });
    });
}

function closeReadline() {
    rl.close();
}

module.exports = {
    handleOperation,
    closeReadline
};
```

**Technical Decisions**:
- Use async/await with Promises for readline operations
- Match COBOL operation codes exactly ('TOTAL', 'CREDIT', 'DEBIT')
- Implement same validation logic as COBOL (insufficient funds check)
- Maintain identical console output messages
- Export closeReadline for cleanup in main module

**Validation**:
- Test each operation type independently
- Verify balance display formatting
- Test credit operations with various amounts
- Test debit with sufficient and insufficient funds
- Validate input handling for invalid amounts

### Phase 4: Main Program Migration

**Objective**: Convert main.cob to main.js

**Current COBOL Implementation** (main.cob):
- Displays menu with 4 options
- Accepts user choice (1-4)
- Calls operations.cob with appropriate parameter
- Loops until user selects exit
- Displays goodbye message

**Node.js Implementation** (main.js):
```javascript
const readline = require('readline');
const operations = require('./operations');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function displayMenu() {
    console.log('--------------------------------');
    console.log('Account Management System');
    console.log('1. View Balance');
    console.log('2. Credit Account');
    console.log('3. Debit Account');
    console.log('4. Exit');
    console.log('--------------------------------');
}

async function processChoice(choice) {
    switch(choice) {
        case '1':
            await operations.handleOperation('TOTAL');
            return true;
        case '2':
            await operations.handleOperation('CREDIT');
            return true;
        case '3':
            await operations.handleOperation('DEBIT');
            return true;
        case '4':
            return false;
        default:
            console.log('Invalid choice, please select 1-4.');
            return true;
    }
}

async function main() {
    let continueRunning = true;
    
    while (continueRunning) {
        displayMenu();
        
        const choice = await new Promise((resolve) => {
            rl.question('Enter your choice (1-4): ', resolve);
        });
        
        continueRunning = await processChoice(choice);
    }
    
    console.log('Exiting the program. Goodbye!');
    operations.closeReadline();
    rl.close();
}

// Start the application
main().catch(console.error);
```

**Technical Decisions**:
- Use async/await for sequential operation execution
- Maintain identical menu structure and messages
- Implement same loop logic with continue flag
- Proper cleanup of readline interfaces on exit
- Match COBOL's synchronous, sequential behavior

**Validation**:
- Test menu display and navigation
- Verify all menu options work correctly
- Test loop continuation after each operation
- Confirm proper exit and cleanup
- Validate invalid input handling

### Phase 5: Testing and Validation

**Objective**: Create and execute comprehensive tests

**Test Approach**:
- Map each test case from TESTPLAN.md to automated tests
- Create unit tests for individual modules
- Create integration tests for full workflow
- Manual testing for user experience validation

**Test Coverage Requirements**:

1. **Data Module Tests** (data.js):
   - Initial balance verification
   - Read operation
   - Write operation
   - Multiple read/write cycles

2. **Operations Module Tests** (operations.js):
   - View balance operation
   - Credit with valid amount
   - Credit with zero amount
   - Debit with valid amount (sufficient funds)
   - Debit exceeding balance (insufficient funds)
   - Debit with zero amount
   - Invalid input handling

3. **Main Program Tests** (main.js):
   - Menu display
   - Each menu option selection
   - Invalid choice handling
   - Exit option and cleanup

4. **Integration Tests**:
   - Complete user workflow scenarios
   - Cross-module data flow validation
   - State persistence across operations

**Test Framework Setup**:
```json
{
  "name": "node-accounting-app",
  "version": "1.0.0",
  "scripts": {
    "start": "node main.js",
    "test": "jest",
    "test:watch": "jest --watch"
  },
  "devDependencies": {
    "jest": "^29.0.0"
  }
}
```

### Phase 6: Documentation and Refinement

**Objective**: Complete documentation and ensure educational value

**Documentation Deliverables**:
1. **Updated README.md**:
   - Add Node.js setup instructions
   - Document side-by-side COBOL vs Node.js comparison
   - Include GitHub Copilot usage examples

2. **Migration Guide**:
   - Step-by-step conversion process
   - COBOL to JavaScript mapping guide
   - Common patterns and translations

3. **Architecture Documentation**:
   - Module interaction diagrams
   - Data flow visualization
   - Comparison charts

4. **Code Comments**:
   - Inline comments explaining COBOL equivalents
   - Rationale for technical decisions
   - References to original COBOL line numbers

## Dependencies and Constraints

### Core Dependencies
```json
{
  "dependencies": {
    "readline": "built-in"
  },
  "devDependencies": {
    "jest": "^29.0.0" (for testing)
  }
}
```

### Constitution Compliance

This implementation plan adheres to all articles of the project constitution:

- ✅ **Article I**: Original COBOL files remain unchanged
- ✅ **Article II**: Test-driven approach with TESTPLAN.md as foundation
- ✅ **Article III**: Three-module architecture preserved
- ✅ **Article IV**: Educational focus with comprehensive documentation
- ✅ **Article V**: Functional equivalence validated through testing
- ✅ **Article VI**: GitHub Copilot workflow demonstrated
- ✅ **Article VII**: Comprehensive markdown documentation
- ✅ **Article VIII**: Minimal dependencies (only Node.js built-ins + testing)
- ✅ **Article IX**: Console interface maintained
- ✅ **Article X**: Incremental, phased implementation

## Risk Management

### Identified Risks

1. **Async/Sync Mismatch**
   - Risk: Node.js async I/O differs from COBOL synchronous model
   - Mitigation: Use async/await to maintain sequential flow
   - Status: Mitigated in design

2. **Floating Point Precision**
   - Risk: JavaScript floating-point arithmetic may differ from COBOL
   - Mitigation: Use .toFixed(2) for display, consider decimal library if needed
   - Status: Acceptable for educational purposes

3. **State Management**
   - Risk: Node.js module caching behavior could affect state
   - Mitigation: Explicit state management in data module
   - Status: Mitigated in design

4. **User Input Handling**
   - Risk: readline interface differs from COBOL ACCEPT
   - Mitigation: Wrap in Promises for consistent async handling
   - Status: Mitigated in design

## Success Metrics

The implementation will be considered successful when:

1. ✅ All three modules converted and functional
2. ✅ All test cases from TESTPLAN.md pass
3. ✅ Side-by-side execution produces identical results
4. ✅ Code is well-documented and educational
5. ✅ No external dependencies beyond Node.js core modules (except testing)
6. ✅ README includes complete setup and usage instructions
7. ✅ Project demonstrates effective GitHub Copilot workflow

## Timeline Estimate

- Phase 1: Project Setup (Complete)
- Phase 2: Data Layer (1 hour)
- Phase 3: Operations Layer (2 hours)
- Phase 4: Main Program (1 hour)
- Phase 5: Testing (2 hours)
- Phase 6: Documentation (2 hours)

**Total Estimated Effort**: 8 hours (excluding Phase 1)

## Next Steps

1. Begin Phase 2: Implement data.js module
2. Create unit tests for data module
3. Validate against TESTPLAN.md test cases
4. Proceed to Phase 3 upon successful validation

## References

- COBOL source files: main.cob, operations.cob, data.cob
- Test documentation: TESTPLAN.md
- Project specification: .specify/specs/spec.md
- Project constitution: .specify/memory/constitution.md
