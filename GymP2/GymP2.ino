#include <SPI.h>
#include <Wire.h>

// pins
// #define A 4
// #define B 5
// #define SELECT 6
// #define START 7
// #define UP 8
// #define DOWN 9
// #define LEFT 10
// #define RIGHT 11

#define LATCH 2 // OUT
#define CLOCK 3 // CLK
#define D0 13   // D0

#define IN_BITS 8
#define OUT_BYTES 4

volatile int pulse; // count to 32 initially, then 64

volatile unsigned long outBuffer;
volatile unsigned long inBuffer;

const unsigned long EXPECTED = 0xFF00FF00;

const unsigned long _ID = 0xFEEDFACE;

const unsigned long ARDUINO_ID = ~_ID << 1;
const unsigned long ARDUINO_ID_START = ~_ID >> 31;

// const byte BUTTONS[8] = {
//     RIGHT, LEFT, DOWN, UP, START, SELECT, B, A,
//
// };

long lastMillis = 0;
int lastValue = 0;

void reset() {
  digitalWrite(D0, ARDUINO_ID_START);
  pulse = 31;
  outBuffer = 0;
  inBuffer = 0;
}

void setup() {
  reset();
  Serial.begin(115200);

  pinMode(LATCH, INPUT);
  pinMode(CLOCK, INPUT);
  pinMode(D0, OUTPUT);
  attachInterrupt(digitalPinToInterrupt(CLOCK), clockPulse, FALLING);

  while (!Serial)
    ;
}

void clockPulse() {
  // pulse expected 0-63 (31 for intial testing)
  // byte phase = pulse >> 5; // expected 0-1
  // byte n = pulse & 0x1F;
  // switch (phase) {
  // case 0: // validation
  //   if (bitRead(EXPECTED, n) != digitalRead(LATCH)) {
  //     reset();
  //     return;
  //   }
  byte newBit = digitalRead(LATCH);
  if (bitRead(EXPECTED, pulse) != newBit) {
    reset();
    return;
  }
  bitWrite(inBuffer, pulse, digitalRead(LATCH));
  digitalWrite(D0, bitRead(ARDUINO_ID, pulse));
  // case 1: // communication
  //   break;
  // }
  pulse--;
}

void loop() {
  reset();
  while (pulse > 0)
    ;
  delay(3);
  long mils = millis();
  long diff = mils - lastMillis;
  lastMillis = mils;
  Serial.print(inBuffer >> 28, HEX);
  Serial.print(inBuffer >> 24 & 0xF, HEX);
  Serial.print(inBuffer >> 24 & 0xF, HEX);
  Serial.print(inBuffer >> 20 & 0xF, HEX);
  Serial.print(inBuffer >> 16 & 0xF, HEX);
  Serial.print(inBuffer >> 12 & 0xF, HEX);
  Serial.print(inBuffer >> 8 & 0xF, HEX);
  Serial.print(inBuffer >> 4 & 0xF, HEX);
  Serial.print(inBuffer & 0xF, HEX);
  Serial.print(" ");
  Serial.println(diff);
}
