/**
 * Integration tests for the complete accounting system
 * 
 * These tests validate end-to-end workflows and cross-module integration
 * based on TESTPLAN.md scenarios.
 */

const data = require('./data');

describe('Integration Tests - Complete Workflows', () => {
    
    beforeEach(() => {
        // Reset to initial state before each test
        data.writeBalance(1000.00);
    });

    describe('Complete User Session Simulation', () => {
        test('should handle multiple operations in sequence', () => {
            // Initial state
            expect(data.readBalance()).toBe(1000.00);
            
            // User views balance (TC-1.1)
            const balance1 = data.readBalance();
            expect(balance1).toBe(1000.00);
            
            // User credits 200.00 (TC-2.1)
            const currentBalance2 = data.readBalance();
            const newBalance2 = currentBalance2 + 200.00;
            data.writeBalance(newBalance2);
            expect(data.readBalance()).toBe(1200.00);
            
            // User debits 150.00 (TC-3.1)
            const currentBalance3 = data.readBalance();
            const newBalance3 = currentBalance3 - 150.00;
            data.writeBalance(newBalance3);
            expect(data.readBalance()).toBe(1050.00);
            
            // User views balance again
            const finalBalance = data.readBalance();
            expect(finalBalance).toBe(1050.00);
        });

        test('should maintain balance integrity across multiple operations', () => {
            // Series of credits
            data.writeBalance(data.readBalance() + 100.00); // 1100.00
            data.writeBalance(data.readBalance() + 50.00);  // 1150.00
            data.writeBalance(data.readBalance() + 25.00);  // 1175.00
            expect(data.readBalance()).toBe(1175.00);
            
            // Series of debits
            data.writeBalance(data.readBalance() - 100.00); // 1075.00
            data.writeBalance(data.readBalance() - 75.00);  // 1000.00
            expect(data.readBalance()).toBe(1000.00);
        });
    });

    describe('Business Logic Validation', () => {
        test('should prevent overdraft when attempting to debit more than balance', () => {
            const currentBalance = data.readBalance(); // 1000.00
            const attemptedDebit = 1500.00;
            
            // Check if sufficient funds (TC-3.2)
            if (currentBalance >= attemptedDebit) {
                data.writeBalance(currentBalance - attemptedDebit);
            }
            // If insufficient, balance should remain unchanged
            
            expect(data.readBalance()).toBe(1000.00); // Balance unchanged
        });

        test('should allow debit when balance is exactly equal to debit amount', () => {
            data.writeBalance(100.00); // Set balance to 100
            const currentBalance = data.readBalance();
            const debitAmount = 100.00;
            
            if (currentBalance >= debitAmount) {
                data.writeBalance(currentBalance - debitAmount);
            }
            
            expect(data.readBalance()).toBe(0.00);
        });

        test('should handle zero amount credit correctly (TC-2.2)', () => {
            const initialBalance = data.readBalance();
            const creditAmount = 0.00;
            
            data.writeBalance(initialBalance + creditAmount);
            
            expect(data.readBalance()).toBe(initialBalance);
        });

        test('should handle zero amount debit correctly (TC-3.3)', () => {
            const initialBalance = data.readBalance();
            const debitAmount = 0.00;
            
            data.writeBalance(initialBalance - debitAmount);
            
            expect(data.readBalance()).toBe(initialBalance);
        });
    });

    describe('Data Persistence Across Operations', () => {
        test('should maintain state throughout session', () => {
            // Operation 1: Credit
            let balance = data.readBalance();
            balance += 500.00;
            data.writeBalance(balance);
            
            // Operation 2: Read (should reflect previous change)
            expect(data.readBalance()).toBe(1500.00);
            
            // Operation 3: Debit
            balance = data.readBalance();
            balance -= 300.00;
            data.writeBalance(balance);
            
            // Operation 4: Read (should reflect all changes)
            expect(data.readBalance()).toBe(1200.00);
        });
    });

    describe('Edge Cases', () => {
        test('should handle very small amounts', () => {
            data.writeBalance(data.readBalance() + 0.01);
            expect(data.readBalance()).toBe(1000.01);
        });

        test('should handle large amounts', () => {
            data.writeBalance(data.readBalance() + 99999.99);
            expect(data.readBalance()).toBe(100999.99);
        });

        test('should handle decimal precision correctly', () => {
            data.writeBalance(1234.56);
            expect(data.readBalance()).toBe(1234.56);
            
            data.writeBalance(data.readBalance() + 0.01);
            expect(data.readBalance()).toBe(1234.57);
        });
    });
});
