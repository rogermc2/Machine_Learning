
package body Examples is

    procedure Init_Binary is
        Bin_1 : constant Binary_Array (1 .. 1) := (1 => 0);
        Bin_2 : constant Binary_Matrix (1 .. 2, 1 .. 1) :=
                  (1 => (1 => 0), 2 => (1 => 1));
    begin
        Binary_Examples.B_Binary.Append ((0, 1));
        Binary_Examples.B_Binary.Append ((1, 1));
        Binary_Examples.B_Binary.Append (Bin_1);
        Binary_Examples.B_Binary_Mat.Append (Bin_2);

        Binary_Examples.B_Integer.Append ((0, 1, 1, 1, 0, 0, 0, 1, 1, 1));
        Binary_Examples.B_Integer.Append ((1, -1));
        Binary_Examples.B_Integer.Append ((3, 5));

    end Init_Binary;

    --  ------------------------------------------------------------------------

    procedure Init_Multiclass is
        Mat_1 : constant Integer_Matrix (1 .. 3, 1 .. 1) :=
                  ((1 => 1), (1 => 0), (1 => 2));
    begin
        Multiclass_Examples.MC_Integer_Array.Append
          ((1, 0, 2, 2, 1, 4, 2, 4, 4, 4));
        Multiclass_Examples.MC_Integer_Array.Append ((1, 0, 2));
        Multiclass_Examples.MC_Integer_Matrix.Append (Mat_1);

        Multiclass_Examples.MC_Strings.Append ("a");
        Multiclass_Examples.MC_Strings.Append ("b");
        Multiclass_Examples.MC_Strings.Append ("c");

    end Init_Multiclass;

    --  ------------------------------------------------------------------------

    procedure Init_Multiclass_Multioutput is
        Mat_1 : constant Integer_Matrix (1 .. 1, 1 .. 3) := (1 => (1, 0, 2));
    begin
        Multiclass_Multioutput_Examples.MCO_Integer.Append
        (((1, 0, 2, 2), (1, 4, 2, 4)));
        Multiclass_Multioutput_Examples.MCO_Integer.Append (Mat_1);

        Multiclass_Multioutput_Examples.MCO_Strings.Append
          ((("a", "b"), ("c", "d")));

    end Init_Multiclass_Multioutput;

    --  ------------------------------------------------------------------------

    procedure Init_Multilabel_Indicators is
        Bin_1 : constant Binary_Matrix (1 .. 1, 1 .. 2) := (others => (0, 1));
    begin
        Multilabel_Indicator_Examples.MI_Binary.Append (((0, 1), (1, 0)));
        Multilabel_Indicator_Examples.MI_Binary.Append (Bin_1);
        Multilabel_Indicator_Examples.MI_Binary.Append (((0, 0), (0, 0)));
        Multilabel_Indicator_Examples.MI_Boolean.Append (((False, True),
                                                      (True, False)));
        Multilabel_Indicator_Examples.MI_Float.Append (((0.0, 1.0), (1.0, 0.0)));
        Multilabel_Indicator_Examples.MI_Integer.Append (((0, 1), (1, 0)));
        Multilabel_Indicator_Examples.MI_Integer.Append (((-1, 1), (1, -1)));
        Multilabel_Indicator_Examples.MI_Integer.Append (((-3, 3), (3, -3)));

    end Init_Multilabel_Indicators;

    --  ------------------------------------------------------------------------

    procedure Init is
    begin
        Init_Binary;
        Init_Multiclass;
        Init_Multiclass_Multioutput;
        Init_Multilabel_Indicators;
    end Init;

end Examples;
