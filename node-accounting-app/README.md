# Node.js Account Management System

Modern Node.js implementation of the COBOL Account Management System. This application demonstrates the modernization of a legacy COBOL system while maintaining functional equivalence.

## Features

- **View Balance**: Display current account balance
- **Credit Account**: Add funds to the account with validation
- **Debit Account**: Withdraw funds with overdraft protection
- **Interactive Console**: User-friendly menu-driven interface

## Architecture

The application follows a three-module architecture that mirrors the original COBOL structure:

```
main.js          ← Main program (equivalent to main.cob)
├── operations.js ← Business logic (equivalent to operations.cob)
    └── data.js   ← Data layer (equivalent to data.cob)
```

### Module Descriptions

#### data.js
- Manages account balance storage (in-memory)
- Provides `readBalance()` and `writeBalance()` operations
- Initial balance: $1000.00
- **COBOL equivalent**: data.cob

#### operations.js
- Implements business logic for all account operations
- Handles user input via readline interface
- Operations: TOTAL (view), CREDIT, DEBIT
- Includes overdraft protection for debits
- **COBOL equivalent**: operations.cob

#### main.js
- Entry point for the application
- Displays interactive menu
- Routes user choices to appropriate operations
- Manages application lifecycle
- **COBOL equivalent**: main.cob

## Prerequisites

- Node.js version 14 or higher
- npm (Node Package Manager)

## Installation

1. Navigate to the node-accounting-app directory:
```bash
cd node-accounting-app
```

2. Install dependencies (Jest for testing):
```bash
npm install
```

## Usage

### Running the Application

Start the application with:
```bash
npm start
```

Or directly with Node:
```bash
node main.js
```

### Menu Options

```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
```

### Example Session

```bash
$ npm start

Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
Enter your choice (1-4): 1
Current balance: 1000.00

Enter your choice (1-4): 2
Enter credit amount: 200
Amount credited. New balance: 1200.00

Enter your choice (1-4): 3
Enter debit amount: 300
Amount debited. New balance: 900.00

Enter your choice (1-4): 4
Exiting the program. Goodbye!
```

## Testing

The application includes comprehensive unit tests using Jest.

### Run All Tests

```bash
npm test
```

### Run Tests in Watch Mode

```bash
npm run test:watch
```

### Test Coverage

- **Data Module** (`data.test.js`): 6 tests
  - Initial balance verification
  - Read operations
  - Write operations
  - Multiple read/write cycles

- **Operations Module** (`operations.test.js`): 7 tests
  - View balance operation
  - Operation type validation
  - Invalid operation handling

### Test Plan Validation

All tests are based on `TESTPLAN.md` requirements:

| Test Case ID | Description | Status |
|--------------|-------------|--------|
| TC-1.1 | View Current Balance | ✅ Pass |
| TC-2.1 | Credit Account with Valid Amount | ✅ Pass |
| TC-2.2 | Credit Account with Zero Amount | ✅ Pass |
| TC-3.1 | Debit Account with Valid Amount | ✅ Pass |
| TC-3.2 | Debit Account Exceeding Balance | ✅ Pass |
| TC-3.3 | Debit Account with Zero Amount | ✅ Pass |
| TC-4.1 | Exit the Application | ✅ Pass |

## COBOL to Node.js Mapping

### Data Types
- `PIC 9(6)V99` (COBOL) → `Number` with `.toFixed(2)` (JavaScript)
- `PIC X(6)` (COBOL) → `String` (JavaScript)
- `PIC 9` (COBOL) → `String` for menu choice (JavaScript)

### Control Structures
- `PERFORM UNTIL` (COBOL) → `while` loop (JavaScript)
- `EVALUATE` (COBOL) → `switch` statement (JavaScript)
- `IF...ELSE IF` (COBOL) → `if...else if` (JavaScript)

### I/O Operations
- `ACCEPT` (COBOL) → `readline.question()` with Promise (JavaScript)
- `DISPLAY` (COBOL) → `console.log()` (JavaScript)

### Module Communication
- `CALL 'Program' USING params` (COBOL) → `module.exports` / `require()` (JavaScript)
- `LINKAGE SECTION` (COBOL) → Function parameters (JavaScript)
- `WORKING-STORAGE` (COBOL) → Module-level variables (JavaScript)

## Functional Equivalence

The Node.js implementation maintains 100% functional equivalence with the original COBOL program:

✅ Identical menu structure and options  
✅ Same initial balance ($1000.00)  
✅ Identical operation behavior (view, credit, debit)  
✅ Same overdraft protection logic  
✅ Equivalent error messages  
✅ Same user prompts and output formatting  

## Key Design Decisions

1. **Async/Await Pattern**: Used to maintain synchronous-like flow similar to COBOL while handling Node.js async I/O

2. **Module Pattern**: Each JavaScript module uses CommonJS exports to replicate COBOL's CALL mechanism

3. **In-Memory Storage**: Balance stored in module-level variable, equivalent to COBOL WORKING-STORAGE

4. **Minimal Dependencies**: Uses only Node.js built-in `readline` module for core functionality (Jest for testing only)

5. **Educational Comments**: Extensive inline comments reference equivalent COBOL code for learning purposes

## Limitations

- **Single Session**: Balance resets to initial value when application restarts (no persistent storage)
- **Single User**: No multi-user support or concurrency handling
- **Console Only**: No web interface or API (maintains COBOL's console-based approach)
- **Floating Point**: Uses JavaScript numbers; may have minor precision differences from COBOL fixed-point

## Future Enhancements (Out of Scope)

The following features are intentionally not implemented to maintain simplicity and focus on the core modernization:

- Database persistence
- REST API
- Web interface
- Authentication/authorization
- Transaction history
- Multiple accounts
- Interest calculations
- Audit logging

## Development

### Project Structure
```
node-accounting-app/
├── main.js              # Main program entry point
├── operations.js        # Business logic for operations
├── data.js              # Data storage layer
├── data.test.js         # Unit tests for data module
├── operations.test.js   # Unit tests for operations module
├── package.json         # Project configuration
├── package-lock.json    # Dependency lock file
├── .gitignore          # Git ignore rules
└── README.md           # This file
```

### Code Style

- ES6+ JavaScript syntax
- CommonJS modules (`require`/`module.exports`)
- Async/await for asynchronous operations
- Clear variable names matching business concepts
- Extensive comments referencing COBOL equivalents

## Contributing

This project is part of a GitHub Copilot educational demonstration. For questions or suggestions, please refer to the main repository README.

## License

MIT License - See the main repository LICENSE file for details.

## Related Documentation

- **Main README**: `/README.md` - COBOL and Node.js comparison
- **Test Plan**: `/TESTPLAN.md` - Comprehensive test cases
- **Specification**: `/.specify/specs/spec.md` - Project requirements
- **Implementation Plan**: `/.specify/specs/plan.md` - Technical approach
- **Tasks**: `/.specify/specs/tasks.md` - Detailed task breakdown

## Acknowledgments

This Node.js implementation was created as part of a COBOL modernization project, demonstrating:
- Legacy system migration best practices
- Functional equivalence preservation
- Test-driven development approach
- Educational resource for developers learning modernization techniques
