/**
 * Data Module - Equivalent to data.cob
 * 
 * This module manages the storage and persistence of the account balance.
 * It provides read and write operations similar to the COBOL DataProgram.
 * 
 * COBOL Reference: data.cob
 * - STORAGE-BALANCE in WORKING-STORAGE (line 6)
 * - READ operation (lines 16-17)
 * - WRITE operation (lines 19-20)
 */

// Module-level variable for balance storage (equivalent to COBOL WORKING-STORAGE)
let accountBalance = 1000.00; // Initial balance as defined in data.cob line 6

/**
 * Read the current account balance
 * Equivalent to COBOL: IF OPERATION-TYPE = 'READ' MOVE STORAGE-BALANCE TO BALANCE
 * 
 * @returns {number} The current account balance
 */
function readBalance() {
    return accountBalance;
}

/**
 * Write a new account balance
 * Equivalent to COBOL: IF OPERATION-TYPE = 'WRITE' MOVE BALANCE TO STORAGE-BALANCE
 * 
 * @param {number} newBalance - The new balance to store
 * @returns {number} The updated balance
 */
function writeBalance(newBalance) {
    accountBalance = newBalance;
    return accountBalance;
}

module.exports = {
    readBalance,
    writeBalance
};
