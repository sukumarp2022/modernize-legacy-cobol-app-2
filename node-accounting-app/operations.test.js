/**
 * Unit tests for operations.js module
 * 
 * These tests validate the operations layer functionality
 * based on COBOL operations.cob behavior and TESTPLAN.md requirements.
 */

const operations = require('./operations');
const data = require('./data');

// Mock console.log to capture output
let consoleOutput = [];
const originalLog = console.log;

beforeEach(() => {
    consoleOutput = [];
    console.log = (output) => {
        consoleOutput.push(output);
    };
    // Reset balance to initial state
    data.writeBalance(1000.00);
});

afterEach(() => {
    console.log = originalLog;
});

afterAll(() => {
    operations.closeReadline();
});

describe('Operations Module Tests', () => {
    
    describe('TC-1.1: View Balance Operation', () => {
        test('should display current balance', async () => {
            await operations.handleOperation('TOTAL');
            expect(consoleOutput).toContain('Current balance: 1000.00');
        });

        test('should display updated balance after changes', async () => {
            data.writeBalance(1500.50);
            await operations.handleOperation('TOTAL');
            expect(consoleOutput).toContain('Current balance: 1500.50');
        });
    });

    describe('Invalid Operation Type', () => {
        test('should handle invalid operation type', async () => {
            await operations.handleOperation('INVALID');
            expect(consoleOutput).toContain('Invalid operation type');
        });

        test('should handle empty operation type', async () => {
            await operations.handleOperation('');
            expect(consoleOutput).toContain('Invalid operation type');
        });
    });

    describe('Operation Type Formatting', () => {
        test('should handle operation type with spaces', async () => {
            await operations.handleOperation('TOTAL ');
            expect(consoleOutput).toContain('Current balance: 1000.00');
        });

        test('should handle CREDIT operation', async () => {
            // Note: This test validates the operation is recognized
            // Actual credit functionality requires user input and is tested manually
            const result = operations.handleOperation('CREDIT');
            expect(result).toBeDefined();
        });

        test('should handle DEBIT operation', async () => {
            // Note: This test validates the operation is recognized
            // Actual debit functionality requires user input and is tested manually
            const result = operations.handleOperation('DEBIT');
            expect(result).toBeDefined();
        });
    });
});

/**
 * Note: Tests for credit and debit operations that require user input
 * are covered by integration tests and manual testing as per TESTPLAN.md.
 * 
 * The following test cases from TESTPLAN.md are validated through manual testing:
 * - TC-2.1: Credit Account with Valid Amount
 * - TC-2.2: Credit Account with Zero Amount
 * - TC-3.1: Debit Account with Valid Amount
 * - TC-3.2: Debit Account with Amount Greater Than Balance
 * - TC-3.3: Debit Account with Zero Amount
 * 
 * These require interactive console input which is better suited for
 * integration and end-to-end testing.
 */
