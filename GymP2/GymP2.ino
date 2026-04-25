#include <SPI.h>
#include <Wire.h>

// pins
#define A 4
#define B 5
#define SELECT 6
#define START 7
#define UP 8
#define DOWN 9
#define LEFT 10
#define RIGHT 11

#define LATCH 2 //  aka OUT
#define CLOCK 3

#define IN_BITS 8
#define OUT_BYTES 4

volatile byte bitBuffer[IN_BITS];
volatile byte bitPtr;

volatile byte outBuffer[OUT_BYTES];
volatile byte outPtr;

volatile byte startSend;
volatile byte startRead;
volatile byte syncPtr;

const byte BUTTONS[8] = {
    RIGHT, LEFT, DOWN, UP, START, SELECT, B, A,

};

long lastMillis = 0;

volatile byte syncBuffer[32];
const byte SYNCDATA[32] = {0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0,
                           0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1};

void reset() {
  startSend = 0;
  startRead = 0;
  setControllerOutput(1);
  bitPtr = 0;
  outPtr = 0;
  syncPtr = 0;
  syncPtr = 0;
  for (int i = 0; i < IN_BITS; i++) {
    bitBuffer[i] = 0;
  }
  for (int i = 0; i < 32; i++) {
    syncBuffer[i] = 0;
  }
  byte tmp = 0xEF;
  for (int i = 0; i < OUT_BYTES; i++) {
    outBuffer[i] = tmp;
    tmp++;
  }
}

void setControllerOutput(byte output) {
  byte out = output ^ 0xFF;
  for (int i = 0; i < 8; i++) {
    digitalWrite(BUTTONS[i], out & 1);
    out = out >> 1;
  }
}

void setup() {
  reset();

  Serial.begin(115200);

  pinMode(RIGHT, OUTPUT);
  pinMode(LEFT, OUTPUT);
  pinMode(UP, OUTPUT);
  pinMode(DOWN, OUTPUT);
  pinMode(SELECT, OUTPUT);
  pinMode(START, OUTPUT);
  pinMode(A, OUTPUT);
  pinMode(B, OUTPUT);

  pinMode(LATCH, INPUT);
  pinMode(CLOCK, INPUT);
  while (!Serial)
    ;
}

void latchPulse() {
  switch (outPtr) {
  case 0: // p1 read
    setControllerOutput(outBuffer[outPtr]);
    outPtr++;
    break;
  case 1: // p2 read 1
    setControllerOutput(outBuffer[outPtr]);
    outPtr++;
    break;
  case 2: // p2 read 2
    setControllerOutput(outBuffer[outPtr]);
    outPtr++;
    break;
  case 3: // p2 read 3
    setControllerOutput(outBuffer[outPtr]);
    outPtr++;
    break;
  case 4: // p2 read 4
    outPtr++;
    break;
  case 5: // p2 read finished
    startSend = 1;
    break;
  }
}

// void readSingleBit() {
//   bitBuffer[bitPtr] = digitalRead(LATCH);
//   bitPtr++;
// }

void readSingleBit() {
  switch (startRead) {
  case 0:
    syncBuffer[syncPtr] = digitalRead(LATCH);
    if (syncBuffer[syncPtr] != SYNCDATA[syncPtr]) {
      // reset sequence
      syncPtr = 0;
      return;
    }
    syncPtr++;
    if (syncPtr < 32)
      return;
    startRead = 1;
    break;
  case 1:
    bitBuffer[bitPtr] = digitalRead(LATCH);
    bitPtr++;
    if (bitPtr < 8)
      return;
    startRead = 2;
    break;
  case 2:
    startSend = 1;
  }
}

void loop() {
  reset();
  attachInterrupt(digitalPinToInterrupt(CLOCK), readSingleBit, FALLING);
  while (!digitalRead(LATCH))
    ;
  while (!startSend)
    ;
    detachInterrupt(digitalPinToInterrupt(CLOCK));
  byte value = 0;
  for (int i = 0; i < 8; i++) {
    Serial.print(bitBuffer[i]);
    value |= bitBuffer[i] << i;
  }
  Serial.print(" ");
  Serial.print(value >> 4, HEX);
  Serial.print(value & 0xF, HEX);
  Serial.print(" ");
  long mils = millis();
  Serial.print(mils - lastMillis);
  Serial.print(" ");
  lastMillis = mils;
  Serial.println(" ok!");
  // attachInterrupt(digitalPinToInterrupt(LATCH), latchPulse, RISING);
  // while (!startSend)
  //   ;
  // detachInterrupt(digitalPinToInterrupt(LATCH));
  // attachInterrupt(digitalPinToInterrupt(CLOCK), readSingleBit, FALLING);
  // while (bitPtr < IN_BITS)
  //   ;
  // detachInterrupt(digitalPinToInterrupt(CLOCK));
  // for (int i = 0; i < 1; i++) {
  //   byte value = 0;
  //   for (int j = 0; j < 8; j++) {
  //     value |= bitBuffer[i * 8 + j] << j;
  //   }
  //   Serial.print(value >> 4, HEX);
  //   Serial.print(value & 0xF, HEX);
  //   if (i < (IN_BITS / 8))
  //     Serial.print(" ");
  // }
  // Serial.println();
  // Serial.println(String(outPtr));
  // Serial.println(String(bitPtr));
  //
  // read bytes into the bit buffer here
}
