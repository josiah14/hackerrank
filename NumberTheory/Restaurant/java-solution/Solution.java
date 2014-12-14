import java.io.*;
import java.util.*;
import java.text.*;
import java.math.*;
import java.util.regex.*;


public class Solution {
  private static BufferedReader stdinReader = new BufferedReader(new InputStreamReader(System.in));

  private static void handleException(Exception e, int errorCode) {
    System.out.println(e.getMessage());
    e.printStackTrace();
    System.exit(errorCode);
  }

  private static int getNumLoaves() {
    int numLoaves = 0;
    try {
      numLoaves = Integer.parseInt(stdinReader.readLine());
    } catch (NumberFormatException e) {
      System.out.println("The first line of input must be a string parsable integer representing the number of bread loaves to " + "process.");
      handleException(e, 10);
    } catch (IOException e) {
      handleException(e, 11);
    }

    return numLoaves;
  }

  private static List<List<Integer>> getAllLoaves(int numberOfLoaves) {
    List<List<Integer>> loaves = new ArrayList<List<Integer>>();
    for (int i = 0; i < numberOfLoaves; i++) {
      List<Integer> loaf = new ArrayList<Integer>();
      try {
        for (String numberStr : stdinReader.readLine().split(" ")) {
          loaf.add(Integer.parseInt(numberStr));
        }
      } catch (NumberFormatException e) {
        System.out.println("Each line of input must consist of 2 parsable integers representing the 2D loaf dimensinos.");
        handleException(e, 20);
      } catch (IOException e) {
        handleException(e, 21);
      }
      loaves.add(loaf);
    }

    try {
      stdinReader.close();
    } catch (IOException e) {
      handleException(e, 22);
    }

    return loaves;
  }

  private static Integer computeGreatestCommonDivisor(List<Integer> loaf) {
    Integer width = loaf.get(0);
    Integer length = loaf.get(1);
    while (length != 0) {
      Integer remainder = width % length;
      width = length;
      length = remainder;
    }
    return width;
  }

  private static Integer computeArea(List<Integer> loaf) {
    return loaf.get(0) * loaf.get(1);
  }

  private static Integer getMinNumberOfSlices(List<Integer> loaf) {
    return computeArea(loaf).intValue() / new Double(Math.pow(computeGreatestCommonDivisor(loaf), 2)).intValue();
  }

  private static List<Integer> computeResults(List<List<Integer>> loaves) {
    List<Integer> results = new ArrayList<Integer>();
    for (List<Integer> loaf : loaves) {
      results.add(getMinNumberOfSlices(loaf));
    }
    return results;
  }

  private static void printResults(List<Integer> results) {
    for (Integer result : results) {
      System.out.println(result);
    }
  }

  public static void main(String[] args) {
    printResults(computeResults(getAllLoaves(getNumLoaves())));

    System.exit(0);
  }
}
