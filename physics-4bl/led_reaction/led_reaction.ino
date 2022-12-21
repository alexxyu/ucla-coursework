// LED variables
int ledPin = 9;
int ledBrightness = 125;
int ledState = LOW;

// Button variables
int buttonPin = 2;
int buttonState = LOW;

// Time variables
long randDelay;
long newT;
long oldT;
long deltaT;

void setup() {
  // put your setup code here, to run once:

  // initialize analog pin as an output
  pinMode(ledPin, OUTPUT);

  // initialize button pin as an input
  pinMode(buttonPin, INPUT);

  Serial.begin(9600);

  Serial.println("Prepare. Reaction time measurements will begin shortly!");
  for(int i=0; i<=3; i++) {

    analogWrite(ledPin, ledBrightness);
    delay(250);
    analogWrite(ledPin, 0);
    delay(250);
    
  }

  // Create a random time seed for the random number generation
  // then delay a random amount of time
  randomSeed(analogRead(A0));
  randDelay = random(3000, 6000);
  delay(randDelay);

}

void loop() {
  // put your main code here, to run repeatedly:

  // Always check to see if button is being pressed.
  buttonState = digitalRead(buttonPin);
  
  // Criteria for trial start.
  if (ledState == LOW && buttonState == LOW) {

    // Turn LED on and record time, trial has begun
    analogWrite(ledPin, ledBrightness);
    oldT = millis();
    ledState = HIGH;
    
  }

  // Criteria for trial end.
  if (ledState == HIGH && buttonState == HIGH) {

    // Record time when button was pressed and turn off LED
    newT = millis();
    analogWrite(ledPin, 0);

    // Print to console the time difference between LED on and off
    deltaT = newT - oldT;
    Serial.println(deltaT);

    // Reset trial and wait a random amount of time for next trial
    ledState = LOW;
    randDelay = random(3000, 6000);
    delay(randDelay);
  }

}
