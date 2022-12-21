int pinOut = 9;
bool signalSwitch = true;
long signalCount = 0;
int bufVal = 50;

void setup() {
  pinMode(pinOut, OUTPUT);
  Serial.begin(115200);
}

void loop() {

  if(signalCount % 100 == 0) {
    signalSwitch = !signalSwitch;
  }

  if(signalSwitch == true) {
    digitalWrite(pinOut, HIGH);
  } else {
    digitalWrite(pinOut, LOW);
  }
  
  signalCount = signalCount + 1;
  Serial.write(analogRead(A0)+bufVal);
  
}
