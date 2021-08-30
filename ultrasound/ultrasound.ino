// Pins and variables
const int trigPin = 2;
const int echoPin = 3;

int numRuns = 32000;
int ii=0;
long duration;

void setup() {
  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
  Serial.begin(9600);
}

void loop() {
  // While loop that will record ultrasound
  // time-of-flight data
  while (ii < numRuns) {
    // Clears trigger pin
    digitalWrite(trigPin, LOW);
    delayMicroseconds(2);

    // Send out 10 microsecond chirp
    digitalWrite(trigPin, HIGH);
    delayMicroseconds(10);
    digitalWrite(trigPin, LOW);

    // Reads the echoPin and returns the sound 
    // wave travel time in microseconds
    duration = pulseIn(echoPin, HIGH);

    // Prints the travel time to Serial
    Serial.println(duration);

    delay(20);

    // Add 1 to count variable
    ii = ii + 1;
  }
}
