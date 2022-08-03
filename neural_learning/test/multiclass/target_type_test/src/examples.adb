
package body Examples is

    procedure Init_Multiclass is
        Mat_1 : constant Integer_Matrix (1 .. 3, 1 .. 1) :=
                  ((1 => 1), (1 => 0), (1 => 2));
    begin
        Multiclass_List.MC_Float.Append ((1.0, 0.0, 0.2));
        Multiclass_List.MC_Integer_Array.Append
          ((1, 0, 2, 2, 1, 4, 2, 4, 4, 4));
        Multiclass_List.MC_Integer_Array.Append ((1, 0, 2));
        Multiclass_List.MC_Integer_Matrix.Append (Mat_1);

        Multiclass_List.MC_Strings.Append ("a");
        Multiclass_List.MC_Strings.Append ("b");
        Multiclass_List.MC_Strings.Append ("c");

    end Init_Multiclass;

    --  ------------------------------------------------------------------------

    procedure Init_Multiclass_Multioutput is
        Mat_1 : Integer_Matrix (1 .. 1, 1 .. 3);
    begin
        Mat_1 (1, 1) := 1;
        Mat_1 (1, 2) := 0;
        Mat_1 (1, 3) := 2;

        Multiclass_Multioutput_List.MCO_Float.Append
         (((1.0, 0.0, 2.0, 2.0), (1.0, 4.0, 2.0, 4.0)));
        Multiclass_Multioutput_List.MCO_Integer.Append
        (((1, 0, 2, 2), (1, 4, 2, 4)));
        Multiclass_Multioutput_List.MCO_Integer.Append (Mat_1);

        Multiclass_Multioutput_List.MCO_Strings.Append
          ((("a", "b"), ("c", "d")));

    end Init_Multiclass_Multioutput;

    --  ------------------------------------------------------------------------

    procedure Init_Multilabel_Indicators is
        Bin_1 : constant Binary_Matrix (1 .. 1, 1 .. 2) := (others => (0, 1));
    begin
        Multilabel_Indicator_List.MI_Binary.Append (((0, 1), (1, 0)));
        Multilabel_Indicator_List.MI_Binary.Append (Bin_1);
        Multilabel_Indicator_List.MI_Binary.Append (((0, 0), (0, 0)));
        Multilabel_Indicator_List.MI_Boolean.Append (((False, True),
                                                      (True, False)));
        Multilabel_Indicator_List.MI_Float.Append (((0.0, 1.0), (1.0, 0.0)));
        Multilabel_Indicator_List.MI_Integer.Append (((0, 1), (1, 0)));
        Multilabel_Indicator_List.MI_Integer.Append (((-1, 1), (1, -1)));
        Multilabel_Indicator_List.MI_Integer.Append (((-3, 3), (3, -3)));

    end Init_Multilabel_Indicators;

    --  ------------------------------------------------------------------------

    procedure Init is
    begin
        Init_Multiclass;
        Init_Multiclass_Multioutput;
        Init_Multilabel_Indicators;
    end Init;

end Examples;
