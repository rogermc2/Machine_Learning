
package Load_ARFF_Data.ARFF_IO is

   procedure Read_ARFF_Ada (File_Name : String; Data : out ARFF_Record);
   procedure Save_ARFF (File_Name : String; Data : ARFF_Record);

end Load_ARFF_Data.ARFF_IO;
