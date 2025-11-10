/**
 * Unit tests for data.js module
 * 
 * These tests validate the data storage layer functionality
 * based on the COBOL DataProgram behavior and TESTPLAN.md requirements.
 */

const data = require('./data');

describe('Data Module Tests', () => {
    
    describe('TC-DATA-1: Initial Balance Verification', () => {
        test('should have initial balance of 1000.00', () => {
            const balance = data.readBalance();
            expect(balance).toBe(1000.00);
        });
    });

    describe('TC-DATA-2: Read Operation', () => {
        test('should return current balance', () => {
            const balance = data.readBalance();
            expect(typeof balance).toBe('number');
            expect(balance).toBeGreaterThanOrEqual(0);
        });

        test('should return same balance on multiple reads', () => {
            const balance1 = data.readBalance();
            const balance2 = data.readBalance();
            expect(balance1).toBe(balance2);
        });
    });

    describe('TC-DATA-3: Write Operation', () => {
        test('should update balance correctly', () => {
            const newBalance = 1500.00;
            const result = data.writeBalance(newBalance);
            expect(result).toBe(newBalance);
            expect(data.readBalance()).toBe(newBalance);
        });

        test('should persist balance across operations', () => {
            data.writeBalance(2000.00);
            expect(data.readBalance()).toBe(2000.00);
            
            data.writeBalance(2500.00);
            expect(data.readBalance()).toBe(2500.00);
        });
    });

    describe('TC-DATA-4: Multiple Read/Write Cycles', () => {
        test('should handle multiple read/write cycles correctly', () => {
            // Cycle 1
            data.writeBalance(1000.00);
            expect(data.readBalance()).toBe(1000.00);
            
            // Cycle 2
            const currentBalance = data.readBalance();
            const newBalance = currentBalance + 200.00;
            data.writeBalance(newBalance);
            expect(data.readBalance()).toBe(1200.00);
            
            // Cycle 3
            const balance3 = data.readBalance();
            data.writeBalance(balance3 - 300.00);
            expect(data.readBalance()).toBe(900.00);
        });
    });
});
