/**
 * Main Program - Equivalent to main.cob
 * 
 * This is the main entry point for the account management system.
 * It displays the menu, accepts user input, and routes to appropriate operations.
 * 
 * COBOL Reference: main.cob
 */

const readline = require('readline');
const operations = require('./operations');

// Create readline interface for user input
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

/**
 * Display the main menu
 * Equivalent to COBOL main.cob lines 12-19
 */
function displayMenu() {
    console.log('--------------------------------');
    console.log('Account Management System');
    console.log('1. View Balance');
    console.log('2. Credit Account');
    console.log('3. Debit Account');
    console.log('4. Exit');
    console.log('--------------------------------');
}

/**
 * Process user menu choice
 * Equivalent to COBOL EVALUATE USER-CHOICE (lines 22-33)
 * 
 * @param {string} choice - The user's menu selection
 * @returns {Promise<boolean>} - Returns true to continue, false to exit
 */
async function processChoice(choice) {
    // COBOL: EVALUATE USER-CHOICE (line 22)
    switch(choice) {
        case '1':
            // COBOL: WHEN 1 CALL 'Operations' USING 'TOTAL ' (lines 23-24)
            await operations.handleOperation('TOTAL');
            return true;
        case '2':
            // COBOL: WHEN 2 CALL 'Operations' USING 'CREDIT' (lines 25-26)
            await operations.handleOperation('CREDIT');
            return true;
        case '3':
            // COBOL: WHEN 3 CALL 'Operations' USING 'DEBIT ' (lines 27-28)
            await operations.handleOperation('DEBIT');
            return true;
        case '4':
            // COBOL: WHEN 4 MOVE 'NO' TO CONTINUE-FLAG (lines 29-30)
            return false;
        default:
            // COBOL: WHEN OTHER DISPLAY "Invalid choice..." (lines 31-32)
            console.log('Invalid choice, please select 1-4.');
            return true;
    }
}

/**
 * Main program loop
 * Equivalent to COBOL MAIN-LOGIC (lines 10-36)
 */
async function main() {
    // COBOL: 01 CONTINUE-FLAG PIC X(3) VALUE 'YES' (line 7)
    let continueRunning = true;
    
    // COBOL: PERFORM UNTIL CONTINUE-FLAG = 'NO' (line 11)
    while (continueRunning) {
        displayMenu();
        
        // COBOL: DISPLAY "Enter your choice (1-4): " ACCEPT USER-CHOICE (lines 19-20)
        const choice = await new Promise((resolve) => {
            rl.question('Enter your choice (1-4): ', resolve);
        });
        
        continueRunning = await processChoice(choice);
    }
    
    // COBOL: DISPLAY "Exiting the program. Goodbye!" (line 35)
    console.log('Exiting the program. Goodbye!');
    
    // Cleanup readline interfaces
    operations.closeReadline();
    rl.close();
    
    // COBOL: STOP RUN (line 36)
}

// Start the application
// This is the entry point when running: node main.js
main().catch(console.error);
