// Reaction time experiment of one LED. 
// LED will flash 3 times before the start of the experiment.

// LED variables
int ledPick;
int ledPinR = 9;
int ledPinG = 10;
int ledPinB = 11;
int ledBrightness = 75;

// LED power states
int ledStateR = LOW;
int ledStateG = LOW;

int ledStateB = LOW;

// Button variables
int buttonPinR = 4;
int buttonStateR = LOW;
int buttonPinG = 5;
int buttonStateG = LOW;
int buttonPinB = 6;
int buttonStateB = LOW;

// Time variables
long randDelay;
long newT;
long oldT;
long deltaT;

// The setup function runs once when you press reset or power the board
void setup() {
  // initialize analog pins as an output.
  pinMode(ledPinR, OUTPUT);
  pinMode(ledPinR, OUTPUT);
  pinMode(ledPinR, OUTPUT);
  
  // Initialize button pins as input.
  pinMode(buttonPinR, INPUT);
  pinMode(buttonPinG, INPUT);
  pinMode(buttonPinB, INPUT);

  // Intialize serial communication.
  Serial.begin(9600);

  Serial.println("Prepare. Reaction time measurements will begin shortly!");
  for (int i = 0; i <= 2; i++) {
    analogWrite(ledPinR, ledBrightness);
    analogWrite(ledPinG, ledBrightness);
    analogWrite(ledPinB, ledBrightness);
    delay(250);
    analogWrite(ledPinR, 0);
    analogWrite(ledPinG, 0);
    analogWrite(ledPinB, 0);
    delay(250);
  }
 
  randomSeed(analogRead(A0));
  randDelay = random(3000, 6000);
  delay(randDelay);
}

// the loop function runs over and over again forever
void loop() {

  //Always check to see if button is being pressed.
  buttonStateR = digitalRead(buttonPinR);
  buttonStateG = digitalRead(buttonPinG);
  buttonStateB = digitalRead(buttonPinB);

  // Critera for trial start.
  if (ledStateR == LOW && buttonStateR == LOW && ledStateG == LOW && buttonStateG == LOW && ledStateB == LOW && buttonStateB == LOW) {

    // Pick a random RGB color.
    ledPick = random(0,3);
    
    // Turn the LED on and record time, trial has begun.
    if (ledPick == 0) {
      analogWrite(ledPinR, ledBrightness);   
      oldT = millis();
      ledStateR = HIGH;
      ledStateG = LOW;
      ledStateB = LOW;
    }
    else if (ledPick == 1) {
      analogWrite(ledPinG, ledBrightness);   
      oldT = millis();
      ledStateR = LOW;
      ledStateG = HIGH;
      ledStateB = LOW;
    }
    else {
      analogWrite(ledPinB, ledBrightness);   
      oldT = millis();
      ledStateR = LOW;
      ledStateG = LOW;
      ledStateB = HIGH;
    }
  }
 
  // Criteria for trial end. 
  if (ledStateR == HIGH && buttonStateR == HIGH) {
    // Record time when button was pressed and turn off LED
    newT = millis();
    analogWrite(ledPinR, 0);   

    // Print to console the time differnce between LED turn off to button pressed.
    deltaT = newT - oldT;  
    Serial.print("1;");    
    Serial.println(deltaT);

    // Reset trial and wait a random amount of time for next trial.
    ledStateR = LOW;
    randDelay = random(1000, 3000);
    delay(randDelay);
  }
  else if (ledStateG == HIGH && buttonStateG == HIGH) {
    // Record time when button was pressed and turn off LED
    newT = millis();
    analogWrite(ledPinG, 0);   

    // Print to console the time differnce between LED turn off to button pressed.
    deltaT = newT - oldT;      
    Serial.print("2;");    
    Serial.println(deltaT);

    // Reset trial and wait a random amount of time for next trial.
    ledStateG = LOW;
    randDelay = random(1000, 3000);
    delay(randDelay);
  }
  else if (ledStateB == HIGH && buttonStateB == HIGH) {
    // Record time when button was pressed and turn off LED
    newT = millis();
    analogWrite(ledPinB, 0);   

    // Print to console the time differnce between LED turn off to button pressed.
    deltaT = newT - oldT;      
    Serial.print("3;");    
    Serial.println(deltaT);

    // Reset trial and wait a random amount of time for next trial.
    ledStateB = LOW;
    randDelay = random(1000, 3000);
    delay(randDelay);
  }
}
