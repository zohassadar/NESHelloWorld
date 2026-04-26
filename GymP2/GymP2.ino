#include <SPI.h>
#include <Wire.h>

#define LATCH 2 // OUT
#define CLOCK 3 // CLK
#define D0 13   // D0

#define IN_BITS 8
#define OUT_BYTES 4

volatile byte bitBuffer[IN_BITS];
volatile byte bitPtr;

volatile byte outBuffer[8];
volatile byte inBuffer[8];

volatile byte phase;
volatile byte syncPtr;

const byte EXPECTED[16] = {

    0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1,

};
volatile byte syncBuffer[16];

const byte ARDUINO_ID[16] = {
    1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,

};

const byte ARDUINO_ID_START = 1; // 0xD2 >> 7 ^ 0xFF

long lastMillis = 0;
int lastValue = 0;

void reset() {
  phase = 0;
  bitPtr = 0;
  syncPtr = 0;
  digitalWrite(D0, ARDUINO_ID_START);
}

void setup() {

  Serial.begin(115200);

  pinMode(LATCH, INPUT);
  pinMode(CLOCK, INPUT);
  pinMode(D0, OUTPUT);
  attachInterrupt(digitalPinToInterrupt(CLOCK), clockPulse, FALLING);
  reset();
  while (!Serial)
    ;
}

void clockPulse() {
  switch (phase) {
  case 0:
    syncBuffer[syncPtr] = digitalRead(LATCH);
    if (syncBuffer[syncPtr] != EXPECTED[syncPtr]) {
      // reset sequence
      syncPtr = 0;
      return;
    }
    digitalWrite(D0, ARDUINO_ID[syncPtr]);
    syncPtr++;
    if (syncPtr < 16)
      return;
    phase = 1;
    break;
  case 1:
    phase = 2;
    break;
  case 2:
    phase = 3;
    break;
  }
}

void loop() {
  reset();
  while (!digitalRead(LATCH))
    ;
  while (phase != 1)
    ;
  delay(3);
  long mils = millis();
  long diff = mils - lastMillis;
  lastMillis = mils;
  Serial.print(syncBuffer[0], HEX);
  Serial.print(syncBuffer[1], HEX);
  Serial.print(syncBuffer[2], HEX);
  Serial.print(syncBuffer[3], HEX);
  Serial.print(syncBuffer[4], HEX);
  Serial.print(syncBuffer[5], HEX);
  Serial.print(syncBuffer[6], HEX);
  Serial.print(syncBuffer[7], HEX);
  Serial.print(syncBuffer[8], HEX);
  Serial.print(syncBuffer[9], HEX);
  Serial.print(syncBuffer[10], HEX);
  Serial.print(syncBuffer[11], HEX);
  Serial.print(syncBuffer[12], HEX);
  Serial.print(syncBuffer[13], HEX);
  Serial.print(syncBuffer[14], HEX);
  Serial.print(syncBuffer[15], HEX);
  Serial.print(" ");
  Serial.println(diff);
}
