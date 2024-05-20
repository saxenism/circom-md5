pragma circom 2.1.6;

include "node_modules/circomlib/circuits/gates.circom";
include "node_modules/circomlib/circuits/bitify.circom";
include "node_modules/circomlib/circuits/comparators.circom";

template BitwiseAND() {
    signal input in[2];
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];
    signal Bb[32];

    component bitwiseAnd[32];
    component num2Bits[2];

    num2Bits[0] = Num2Bits(32);
    num2Bits[1] = Num2Bits(32);

    num2Bits[0].in <== in[0];
    num2Bits[1].in <== in[1];

    Ab <== num2Bits[0].out;
    Bb <== num2Bits[1].out;

    for(var i = 0; i < 32; i++) {
        bitwiseAnd[i] = AND();

        bitwiseAnd[i].a <== Ab[i];
        bitwiseAnd[i].b <== Bb[i];

        out[i] <== bitwiseAnd[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}

template BitwiseOR() {
    signal input in[2];
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];
    signal Bb[32];

    component bitwiseOr[32];
    component num2Bits[2];

    num2Bits[0] = Num2Bits(32);
    num2Bits[1] = Num2Bits(32);

    num2Bits[0].in <== in[0];
    num2Bits[1].in <== in[1];

    Ab <== num2Bits[0].out;
    Bb <== num2Bits[1].out;

    for(var i = 0; i < 32; i++) {
        bitwiseOr[i] = OR();

        bitwiseOr[i].a <== Ab[i];
        bitwiseOr[i].b <== Bb[i];

        out[i] <== bitwiseOr[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}

template BitwiseNOT() {
    signal input in;
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];

    component bitwiseNot[32];
    component num2Bits = Num2Bits(32);

    num2Bits.in <== in;

    Ab <== num2Bits.out;

    for(var i = 0; i < 32; i++) {
        bitwiseNot[i] = NOT();

        bitwiseNot[i].in <== Ab[i];
        out[i] <== bitwiseNot[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}   

template BitwiseXOR() {
    signal input in[2];
    signal output out[32];
    signal output decimalOut;

    signal Ab[32];
    signal Bb[32];

    component bitwiseXor[32];
    component num2Bits[2];

    num2Bits[0] = Num2Bits(32);
    num2Bits[1] = Num2Bits(32);

    num2Bits[0].in <== in[0];
    num2Bits[1].in <== in[1];

    Ab <== num2Bits[0].out;
    Bb <== num2Bits[1].out;

    for(var i = 0; i < 32; i++) {
        bitwiseXor[i] = XOR();

        bitwiseXor[i].a <== Ab[i];
        bitwiseXor[i].b <== Bb[i];

        out[i] <== bitwiseXor[i].out;
    }

    component bits2Num = Bits2Num(32);
    bits2Num.in <== out;
    decimalOut <== bits2Num.out;
}

template CombineLoop1() {
/*
return (B & C) | (~B & D)
*/
    signal input B, C, D;
    signal output out;
    
    signal exp1;
    signal exp2;
    signal exp3;

    component band[2];

    component bnot = BitwiseNOT();
    band[0] = BitwiseAND();
    band[1] = BitwiseAND();
    component bor = BitwiseOR();

    bnot.in <== B;
    exp1 <== bnot.decimalOut;
    
    band[0].in[0] <== exp1;
    band[0].in[1] <== D;

    exp2 <== band[0].decimalOut;

    band[1].in[0] <== B;
    band[1].in[1] <== C;

    exp3 <== band[1].decimalOut;

    bor.in[0] <== exp2;
    bor.in[1] <== exp3;

    out <== bor.decimalOut;
}

template CombineLoop2() {
    signal input B, C, D;
    signal output out <== 1;
}

template CombineLoop3() {
    signal input B, C, D;
    signal output out <== 1;
}

template CombineLoop4() {
    signal input B, C, D;
    signal output out <== 1;
}

template Md5() {
    // since the word size in the MD5 algo is 32 bits and the input size is 512, let's create an input signal of 16 elements.
    signal input in[16];
    signal input out[4]; // The hash result

    ////////////////////////////////
    /// Range Check on the input in
    ////////////////////////////////
    // We need to constrain the input signals to be less than 2**32.
    component isLessEqThan[16];
    signal rangeCheckArray[16];

    for(var i = 0; i < 16; i++) {
        isLessEqThan[i] = LessEqThan(32);

        isLessEqThan[i].in[0] <== 0;
        isLessEqThan[i].in[1] <== ((in[i] - 0)*(2**32 - in[i]));

        rangeCheckArray[i] <== isLessEqThan[i].out;
    }

    // Now we need to make sure that all the entries in the rangeCheckArray are 1.
    // So, we will do an AND of all the value in rangeCheckArray and constraint that output to be equal to 1.
    component multipleANDs = MultiAND(16);
    multipleANDs.in <== rangeCheckArray;
    multipleANDs.out === 1;

    /////////////////////////////////////////////
    /// Loop iterations for state change starts
    /////////////////////////////////////////////

    signal states[64][4]; // since there are going to be 64 states and each state plays with 4 values.

    // Initial state value is same for all MD5 hashes
    states[0][0] <== 0x67452301;
    states[0][1] <== 0xefcdab89;
    states[0][2] <== 0x98badcfe;
    states[0][3] <== 0x10325476;

    component combine1[16];
    component combine2[16];
    component combine3[16];
    component combine4[16];

    // First loop of state changes
    for(var i = 1; i < 16; i++) {
        combine1[i] = CombineLoop1();

        combine1[i].B <== states[i-1][1];
        combine1[i].C <== states[i-1][2];
        combine1[i].D <== states[i-1][3];

        states[i][0] <== states[i-1][3];
        states[i][1] <== combine1[i].out;
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Second loop of state changes
    var c2_idx = 0;
    for(var j = 16; j < 32; j++) {
        combine2[c2_idx] = CombineLoop2();

        combine2[c2_idx].B <== states[j-1][1];
        combine2[c2_idx].C <== states[j-1][2];
        combine2[c2_idx].D <== states[j-1][3];

        states[j][0] <== states[j-1][3];
        states[j][1] <== combine2[c2_idx].out;
        states[j][2] <== states[j-1][1];
        states[j][3] <== states[j-1][2];

        c2_idx++;
    }

    // Third loop of state changes
    var c3_idx = 0;
    for(var l = 32; l < 48; l++) {
        combine3[c3_idx] = CombineLoop3();

        combine3[c3_idx].B <== states[l-1][1];
        combine3[c3_idx].C <== states[l-1][2];
        combine3[c3_idx].D <== states[l-1][3];

        states[l][0] <== states[l-1][3];
        states[l][1] <== combine3[c3_idx].out;
        states[l][2] <== states[l-1][1];
        states[l][3] <== states[l-1][2];

        c3_idx++;
    }

    // Fourth loop of state changes
    var c4_idx = 0;
    for(var k = 48; k < 64; k++) {
        combine4[c4_idx] = CombineLoop4();

        combine4[c4_idx].B <== states[k-1][1];
        combine4[c4_idx].C <== states[k-1][2];
        combine4[c4_idx].D <== states[k-1][3];

        states[k][0] <== states[k-1][3];
        states[k][1] <== combine4[c4_idx].out;
        states[k][2] <== states[k-1][1];
        states[k][3] <== states[k-1][2];

        c4_idx++;
    }

    // Making sure that the last state change corresponds to the hash output inputted to the circuit
    out[0] === states[63][0];
    out[1] === states[63][1];
    out[2] === states[63][2];
    out[3] === states[63][3];
}

component main{ public [ out ] } = Md5();