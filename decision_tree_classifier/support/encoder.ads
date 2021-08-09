
with Classifier_Types; use Classifier_Types;
with ML_Types;

package Encoder is

    type Label_Encoder is record
    --  Encode target labels with value between 0 and n_classes-1.
    --  This transformer should be used to encode target values, *i.e.* `y`, and
    --  not the input `X`.
        Classes : Integer_List;
    end record;

    type Hot_Encoder_Parameters is record
        Auto_Category        : Boolean := False;
        Categories           : ML_Types.Value_Data_List;
        Drop_First           : Boolean := False;
        Drop                 : ML_Types.Value_Data_List;
        Data_Kind            : ML_Types.Data_Type;
        Num_Values_Auto      : Boolean := True;
        Num_Values           : Integer_List;
        All_Cat_Features     : Boolean := True;
        Categorical_Features : ML_Types.Value_Data_List;
    end record;

    type Hot_Encoder_Attributes is record
        Categories : ML_Types.Value_Data_List;
        Drop_Index : ML_Types.Value_Data_List;
        Num_Values : Integer_List;
    end record;

    type One_Hot_Encoder is record
        Parameters : Hot_Encoder_Parameters;
        Attributes : Hot_Encoder_Attributes;
    end record;

    type Ordinal_Encoder_Parameters is record
        Auto_Category        : Boolean := False;
        Categories           : ML_Types.Value_Data_List;
        Data_Kind            : ML_Types.Data_Type;
    end record;

    type Ordinal_Encoder_Attributes is record
        Categories : ML_Types.Value_Data_List;
    end record;

    type Ordinal_Hot_Encoder is record
        Parameters : Ordinal_Encoder_Parameters;
        Attributes : Ordinal_Encoder_Attributes;
    end record;

    function Fit (Self : Label_Encoder; Y : ML_Types.Value_Data_List)
                  return Label_Encoder;
    function Fit_Transform (Self : in out Label_Encoder;
                            X    : Sample_Matrix;
                            Y    : ML_Types.Value_Data_List :=
                              ML_Types.Value_Data_Package.Empty_Vector)
                            return Sample_Matrix;
    procedure Init (Self                 : in out One_Hot_Encoder;
                    Categories           : ML_Types.Value_Data_List;
                    Drop                 : ML_Types.Value_Data_List;
                    Data_Kind            : ML_Types.Data_Type;
                    Num_Values           : Integer_List;
                    Categorical_Features : ML_Types.Value_Data_List;
                    Auto_Category        : Boolean := False;
                    Drop_First           : Boolean := False;
                    Num_Values_Auto      : Boolean := True;
                    All_Cat_Features     : Boolean := True);

end Encoder;
