describe("group1", function() {    
    it("runnable (default options; chain of N(10.0,5.0))", function() {
        expect(1).toBe(1);
    });

    it("compiled func", function () {
        expect(compilationTest.addByDima(1, 2)).toBe(3);
    });
});

describe("MT", function () {
    it("simple sequnce reproducibility", function () {
        var rng = MT.MT19937.New1(123);
        var seq1 = [];        
        for (var i = 0; i < 10; i++)
            seq1.push(rng.uniform_uint32());
        var seq2 = [];
        var rng = MT.MT19937.New1(123);
        for (var i = 0; i < 10; i++)
            seq2.push(rng.uniform_uint32());
        for (var i = 0; i < 10; i++)
            expect(seq1[i]).toBe(seq2[i]);
    });
});