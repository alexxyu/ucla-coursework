// Pins
int thermistorPin = A0;
#define ENABLE 5
#define DIRA 3
#define DIRB 4

// Thermistor global variables
double tempK;
float tempC;
float tempF;
int tempReading;

float R1 = 10000.0;
float c1 = 2.055804316e-03, c2 = 0.5779987905e-04, c3 = 5.356844641e-07;

int startTime;

// Motor speed (1 is off, 2 is slow, 3 is fast)
int motorSpeed = 3;

void setup() {
  // Set pin direction
  pinMode(ENABLE, OUTPUT);
  pinMode(DIRA, OUTPUT);
  pinMode(DIRB, OUTPUT);

  startTime = micros();
 
  Serial.begin(115200);
}

void loop() {
  // Thermistor measurements
  tempReading = analogRead(thermistorPin);
 
  // Temperature conversions
  tempK = log(R1 * ((1024.0 / tempReading - 1)));
  // tempK = 1/(0.001129148+(0.000234125+(0.0000000876741*tempK*tempK))* tempK);
  tempK = (1.0 / (c1 + c2*tempK + c3*tempK*tempK*tempK));
  tempC = tempK - 273.15;
  tempF = (tempC * 9.0) / 5.0 + 32.0;
 
  // Printing temperature in Celsius
  //Serial.println(tempC);
  //Serial.print(micros()-startTime);
  //Serial.print(",");
  Serial.println(tempC);

  // Running motor
  if(motorSpeed == 1)
  {
    digitalWrite(ENABLE, LOW);
  }
  else if(motorSpeed == 2)
  {
    analogWrite(ENABLE, 128);
    digitalWrite(DIRA, LOW);
    digitalWrite(DIRB, HIGH);
  }
  else if(motorSpeed == 3)
  {
    digitalWrite(ENABLE, HIGH);
    digitalWrite(DIRA, LOW);
    digitalWrite(DIRB, HIGH);
  }

  delay(50);
}
