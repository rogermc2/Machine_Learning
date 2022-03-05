
package Load_ARFF_Data.ARFF_Printing is

    procedure Print_ARFF (Data : ARFF_Record);
    procedure Print_Attributes (Data : ARFF_Record);
    procedure Print_Attributes (Text : String; Data : Attribute_List);
    procedure Print_Data (Data : ARFF_Record; Start : Positive := 1;
                         Last : Positive := 10);
    procedure Print_Description (Data : ARFF_Record);
    procedure Print_Target (Data : ARFF_Record; Start : Positive := 1;
                            Last : Positive := 10);

end Load_ARFF_Data.ARFF_Printing;
