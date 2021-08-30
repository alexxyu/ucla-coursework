const int V1Pin = A1; //Declares the pin to read for V1
const int V2Pin = A2; //Declares the pin to read for V2

int V1; //analogReading integer for V1
int V2; //analogReading integer for V2

void setup() {
  Serial.begin(115200); //Opens the serial port connection
}

void loop() {
  
  //Reads in analog signals
  V1 = analogRead(V1Pin); //Reads V1 pin as an analog signal int
  V2 = analogRead(V2Pin); //Reads V2 pin as an analog signal int

  //Prints data to serial port
  Serial.print(V1); //Print V1 value
  Serial.print(","); //Prints a comma to allow easy CSV exporting
  Serial.println(V2); //Print V2 value
  delay(25); //Waits 25ms
}
