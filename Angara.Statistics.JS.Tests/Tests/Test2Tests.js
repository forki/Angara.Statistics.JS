describe("group1", function() {    
    it("runnable (default options; chain of N(10.0,5.0))", function() {
        expect(1).toBe(1);
    });

    it("compiled func", function () {
        expect(compilationTest.addByDima(1, 2)).toBe(3);
    });
});