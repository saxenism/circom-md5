pragma circom 2.1.6;

include "node_modules/circomlib/circuits/gates.circom";
include "node_modules/circomlib/circuits/bitify.circom";
include "node_modules/circomlib/circuits/comparators.circom";

template BitwiseAND() {
    signal input in[2];
    signal output out[32];

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
}

template BitwiseOR() {
    signal input in[2];
    signal output out[32];

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
}

template BitwiseNOT() {
    signal input in;
    signal output out[32];

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
}

template BitwiseXOR() {
    signal input in[2];
    signal output out[32];

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
}

template Combine() {
    signal input B, C, D, i;
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

    component combine[64];

    // First loop of state changes
    for(var i = 1; i < 16; i++) {
        combine[i] = Combine();

        combine[i].B <== states[i-1][1];
        combine[i].C <== states[i-1][2];
        combine[i].D <== states[i-1][3];
        combine[i].i <== i;

        states[i][0] <== states[i-1][3];
        states[i][1] <== combine[i].out;
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Second loop of state changes
    for(var j = 16; j < 32; j++) {
        var i = (5 * i + 1) % 16;

        combine[j] = Combine();

        combine[j].B <== states[i-1][1];
        combine[j].C <== states[i-1][2];
        combine[j].D <== states[i-1][3];
        combine[j].i <== i;

        states[i][0] <== states[i-1][3];
        states[i][1] <== combine[j].out;
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Third loop of state changes
    for(var l = 32; l < 48; l++) {
        var i = (3 * i + 5) % 16;

        combine[l] = Combine();

        combine[l].B <== states[i-1][1];
        combine[l].C <== states[i-1][2];
        combine[l].D <== states[i-1][3];
        combine[l].i <== i;

        states[i][0] <== states[i-1][3];
        states[i][1] <== combine[l].out;
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    // Fourth loop of state changes
    for(var k = 48; k < 64; k++) {
        var i = (7 * i) % 16;

        combine[k] = Combine();

        combine[k].B <== states[i-1][1];
        combine[k].C <== states[i-1][2];
        combine[k].D <== states[i-1][3];
        combine[k].i <== i;

        states[i][0] <== states[i-1][3];
        states[i][1] <== combine[k].out;
        states[i][2] <== states[i-1][1];
        states[i][3] <== states[i-1][2];
    }

    out[0] <== states[63][0];
    out[1] <== states[63][1];
    out[2] <== states[63][2];
    out[3] <== states[63][3];

}

component main{ public [ out ] } = Md5();