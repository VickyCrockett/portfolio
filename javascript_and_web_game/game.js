const colors = ["red", "blue", "yellow", "green", "orange"];
let score = 0;
let selectedCards = [];

// Function to generate cards
function generateCards() {
    const cardsContainer = document.getElementById("cards");
    cardsContainer.innerHTML = ""; // Clear existing cards
    for (let i = 0; i < 25; i++) {
        const card = document.createElement("div");
        card.className = "card";
        card.style.backgroundColor = colors[Math.floor(Math.random() * colors.length)];
        card.onclick = () => selectCard(card, i);
        cardsContainer.appendChild(card);
    }
}

// Function to handle card selection
function selectCard(card, index) {
    if (selectedCards.includes(index)) {
        selectedCards = selectedCards.filter((i) => i !== index);
        card.classList.remove("selected");
    } else if (selectedCards.length < 5) {
        selectedCards.push(index);
        card.classList.add("selected");
    }
    // Prevent more than 5 cards from being selected
}

// Function to check if selected cards form a set
function checkForSet() {
    const selectedColors = selectedCards.map((index) => document.getElementsByClassName("card")[index].style.backgroundColor);
    const uniqueColors = [...new Set(selectedColors)];
    if (uniqueColors.length === 1 || uniqueColors.length === 5) {
        score++;
        document.getElementById("score").innerText = "Score: " + score;
        generateCards(); // Regenerate cards for the next round
        selectedCards = [];
    } else {
        // Game over, check if score is a high score
        updateHighScores();
    }
}

// Function to update and display high scores
function updateHighScores() {
    let highScores = JSON.parse(localStorage.getItem("highScores")) || [];
    let playerName = prompt("Game Over! Enter your initials:", "AAA");
    let today = new Date().toISOString().slice(0, 10);
    highScores.push({ score, playerName, date: today });
    highScores.sort((a, b) => b.score - a.score);
    highScores = highScores.slice(0, 5); // Keep top 5 scores
    localStorage.setItem("highScores", JSON.stringify(highScores));
    displayHighScores();
    score = 0; // Reset score for next game
}

// Function to display high scores
function displayHighScores() {
    const highScores = JSON.parse(localStorage.getItem("highScores")) || [];
    const highScoresElement = document.getElementById("highScores");
    highScoresElement.innerHTML = "High Scores:<br>";
    highScores.forEach((score) => {
        highScoresElement.innerHTML += `${score.playerName}: ${score.score} (${score.date})<br>`;
    });
}

document.getElementById("submitSelection").onclick = checkForSet;

// Initialize game
document.addEventListener('DOMContentLoaded', () => {
    console.log("Document loaded");
    generateCards();
    displayHighScores();
});

