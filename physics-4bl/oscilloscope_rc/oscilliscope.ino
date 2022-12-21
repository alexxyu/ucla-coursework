int bufVal = 75; //Virtual DC Offset for better oscilliscope visibility

void setup() {
  Serial.begin(115200);
}

void loop() {
  Serial.write(analogRead(A0)+bufVal);
}
