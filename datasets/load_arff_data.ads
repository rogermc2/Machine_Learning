
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Load_ARFF_Data is

    type ARFF_Record is private;

    procedure Load_ARFF (File_Name : String; Data : out ARFF_Record);

private

    type ARFF_Record is record
        Header : Unbounded_String;
    end record;

end Load_ARFF_Data;
