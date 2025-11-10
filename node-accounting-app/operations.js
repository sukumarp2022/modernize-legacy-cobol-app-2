/**
 * Operations Module - Equivalent to operations.cob
 * 
 * This module handles the business logic for account operations:
 * - View Balance (TOTAL)
 * - Credit Account (CREDIT)
 * - Debit Account (DEBIT)
 * 
 * COBOL Reference: operations.cob
 */

const readline = require('readline');
const data = require('./data');

// Create readline interface for user input
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

/**
 * Handle different operation types
 * Equivalent to COBOL PROCEDURE DIVISION USING PASSED-OPERATION (line 13)
 * 
 * @param {string} operationType - The operation to perform: 'TOTAL', 'CREDIT', or 'DEBIT'
 * @returns {Promise<void>}
 */
async function handleOperation(operationType) {
    // COBOL: MOVE PASSED-OPERATION TO OPERATION-TYPE (line 14)
    const operation = operationType.trim();
    
    // COBOL: EVALUATE or IF statements (lines 16-39)
    if (operation === 'TOTAL') {
        await viewBalance();
    } else if (operation === 'CREDIT') {
        await creditAccount();
    } else if (operation === 'DEBIT') {
        await debitAccount();
    } else {
        console.log('Invalid operation type');
    }
}

/**
 * View current account balance
 * Equivalent to COBOL operations.cob lines 16-18
 * IF OPERATION-TYPE = 'TOTAL'
 */
async function viewBalance() {
    // COBOL: CALL 'DataProgram' USING 'READ', FINAL-BALANCE (line 17)
    const balance = data.readBalance();
    
    // COBOL: DISPLAY "Current balance: " FINAL-BALANCE (line 18)
    console.log(`Current balance: ${balance.toFixed(2)}`);
}

/**
 * Credit the account with a specified amount
 * Equivalent to COBOL operations.cob lines 20-26
 * ELSE IF OPERATION-TYPE = 'CREDIT'
 * 
 * @returns {Promise<void>}
 */
async function creditAccount() {
    return new Promise((resolve) => {
        // COBOL: DISPLAY "Enter credit amount: " (line 21)
        rl.question('Enter credit amount: ', (input) => {
            // COBOL: ACCEPT AMOUNT (line 22)
            const amount = parseFloat(input);
            
            // Input validation (not explicitly in COBOL but good practice)
            if (isNaN(amount) || amount < 0) {
                console.log('Invalid amount');
                resolve();
                return;
            }
            
            // COBOL: CALL 'DataProgram' USING 'READ', FINAL-BALANCE (line 23)
            const currentBalance = data.readBalance();
            
            // COBOL: ADD AMOUNT TO FINAL-BALANCE (line 24)
            const newBalance = currentBalance + amount;
            
            // COBOL: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE (line 25)
            data.writeBalance(newBalance);
            
            // COBOL: DISPLAY "Amount credited. New balance: " FINAL-BALANCE (line 26)
            console.log(`Amount credited. New balance: ${newBalance.toFixed(2)}`);
            resolve();
        });
    });
}

/**
 * Debit the account with overdraft protection
 * Equivalent to COBOL operations.cob lines 28-38
 * ELSE IF OPERATION-TYPE = 'DEBIT'
 * 
 * @returns {Promise<void>}
 */
async function debitAccount() {
    return new Promise((resolve) => {
        // COBOL: DISPLAY "Enter debit amount: " (line 29)
        rl.question('Enter debit amount: ', (input) => {
            // COBOL: ACCEPT AMOUNT (line 30)
            const amount = parseFloat(input);
            
            // Input validation (not explicitly in COBOL but good practice)
            if (isNaN(amount) || amount < 0) {
                console.log('Invalid amount');
                resolve();
                return;
            }
            
            // COBOL: CALL 'DataProgram' USING 'READ', FINAL-BALANCE (line 31)
            const currentBalance = data.readBalance();
            
            // COBOL: IF FINAL-BALANCE >= AMOUNT (line 32)
            if (currentBalance >= amount) {
                // COBOL: SUBTRACT AMOUNT FROM FINAL-BALANCE (line 33)
                const newBalance = currentBalance - amount;
                
                // COBOL: CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE (line 34)
                data.writeBalance(newBalance);
                
                // COBOL: DISPLAY "Amount debited. New balance: " FINAL-BALANCE (line 35)
                console.log(`Amount debited. New balance: ${newBalance.toFixed(2)}`);
            } else {
                // COBOL: ELSE DISPLAY "Insufficient funds for this debit." (line 37)
                console.log('Insufficient funds for this debit.');
            }
            resolve();
        });
    });
}

/**
 * Close the readline interface
 * Required for proper cleanup and to prevent hanging process
 */
function closeReadline() {
    rl.close();
}

module.exports = {
    handleOperation,
    closeReadline
};
