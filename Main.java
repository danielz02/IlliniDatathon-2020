package com.company;

import java.io.File;
import java.io.FileWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
	// write your code here
        generateTargetDataFrame();

    }

    public static void generateRawData() {
        Map<Integer, String> add = new HashMap<>();
        int progress = 0;
        try {
            File originalFile = new File("C:\\Users\\yj17\\Desktop\\datathon\\data.csv");
            File addFile = new File("C:\\Users\\yj17\\Desktop\\datathon\\filename.csv");

            FileWriter myWriter = new FileWriter("C:\\Users\\yj17\\Desktop\\datathon\\fulldata.txt");
            Scanner scannerOriginal = new Scanner(originalFile);
            Scanner scannerAdd = new Scanner(addFile);


            while(scannerAdd.hasNextLine()) {
                String newAddLine = scannerAdd.nextLine();
                add.put(Integer.parseInt(newAddLine.split(",")[0].trim()), newAddLine);


            }


            while (scannerOriginal.hasNextLine()) {
                progress++;
                if (progress % 1000 == 0) {
                    System.out.println((double) progress / 4873355);
                }


                String originalLine = scannerOriginal.nextLine();
                originalLine += ',';
                originalLine += add.get(Integer.parseInt(originalLine.split(",")[2].trim()))                                    ;
                myWriter.write(originalLine + "\n");
            }
            myWriter.close();


        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    public static void generateTargetDataFrame() {
        int index = 0;
        try {

            FileWriter myWriter = new FileWriter("C:\\Users\\yj17\\Desktop\\datathon\\targetDataFrame.csv");
            File originalFile = new File("C:\\Users\\yj17\\Desktop\\datathon\\zip_info_combined.csv");
            Scanner sc = new Scanner(originalFile);


            while (sc.hasNextLine()) {
                String[] str = sc.nextLine().trim().split(",");
                switch (str[str.length - 1]) {
                    case "LA":
                    case "IL":
                    case "FL":
                    case "TX":
                    case "NC":
                        boolean a = true;
                        switch (str[8]) {
                            case "Establishments operated entire year with sales/receipts/revenue of $100,000 to $249,999":
                                str[8] = String.valueOf((100000 + 249999) / 2);
                                break;
                            case "Establishments operated entire year with sales/receipts/revenue of $250,000 to $499,999":
                                str[8] = String.valueOf((250000 + 499999) / 2);
                                break;
                            case "Establishments operated entire year with sales/receipts/revenue of $500,000 to $999,999":
                                str[8] = String.valueOf((500000 + 999999) / 2);
                                break;
                            case "Establishments operated entire year with sales/receipts/revenue of $1,000,000 or more":
                                str[8] = String.valueOf(1000000);
                                break;
                            case "Establishments operated entire year with sales/receipts/revenue less than $100,000":
                                str[8] = String.valueOf(100000);
                                break;
                            default:
                                a = false;
                                break;
                        }
                        if (a) {
                            myWriter.write(arrToCSV(str, index) + "\n");
                            index++;
                        }
                        break;
                    default:
                        break;
                }


            }
            myWriter.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static String arrToCSV(String[] arr, int index) {
        String toReturn = String.valueOf(index);
        for (int i = 1; i < arr.length; i++) {
            toReturn += "," + arr[i];
        }
        return toReturn;
    }
}
