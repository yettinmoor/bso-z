  LDY #$1
  LDA #$5
  STA $5001
  ROR
  ADC $5000,Y
  RTS
