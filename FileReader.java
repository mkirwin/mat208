package utility;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class FileReader {

  public static class Track {
    // TODO didn't want to write getters.
    public String trackTitle;
    public List<String> artists;
    public String trackUri;

    /**
     * Constructor for if you already have the artistList.
     * @param title
     * @param artistList
     * @param trackUri
     */
    public Track(String title, List<String> artistList, String trackUri)  {
      this.trackTitle = title;
      if (artists == null) {
        this.artists = new ArrayList<>();
      }
      this.artists.addAll(artistList);
      this.trackUri = trackUri;
    }

    /**
     * Constructor to add each field one at a time.
     */
    public Track() {
      this.trackTitle = null;
      this.artists = new ArrayList<>();
      this.trackUri = null;
    }

    public void addTitle(String title) {
      this.trackTitle = title;
    }

    public void addArtist(String artist) {
      if (artists == null) { // TODO pick one method of handling this
        this.artists = new ArrayList<>();
      }
      this.artists.add(artist);
    }

    public void addArtistList(List<String> artistList) {
      if (artists == null) {
        this.artists = new ArrayList<>();
      }
      this.artists.addAll(artistList);
    }

    public void addTrackUri(String trackUri) {
      this.trackUri = trackUri;
    }

  }



  public static Track parseTrackFromTextSnippet(List<String> stringFormattedTrack) {
    String trackTitle = "<title-dummy>";
    List<String> trackArtistList = new ArrayList<>();
    String trackUri = "<uri-dummy>";
    Iterator<String> iter = stringFormattedTrack.iterator();
    while (iter.hasNext()) {
      String currentLine = iter.next();
      if (currentLine.startsWith("\"uri\": \"spotify:track:")) {
        System.out.println("Parsing URI...");
        trackUri = parseUri(currentLine);
      } else if (currentLine.startsWith("\"artist_name")) {
        String artistName = parseArtistName(currentLine);
        System.out.println("Parsing artist name (unfinished)... :  " + artistName);
        trackArtistList.add(artistName);
      } else if (currentLine.startsWith("\"title\":")) {
        System.out.println("Parsing title...");
        trackTitle = parseTrackTitle(currentLine);
      }
    }
    Track track = new Track(trackTitle, trackArtistList, trackUri);
    return track;
  }

  /**
   * TODO come back to this.
   * @param currentLine
   * @return
   */
  private static String parseArtistName1(String currentLine) {
    removeArtistPrefix2(currentLine);
    // Remove all "s
    currentLine = currentLine.replaceAll("\",", "");
    return currentLine;
  }

  private static String removeArtistPrefix2(String currentLine) {
    currentLine = currentLine.replaceAll("\"", "");
    return currentLine;
  }

  private static String removeArtistPrefix1(String currentLine) {
    // TODO use regex '\d'
    String artist_regex = "\"artist_name\"";
    String artist0_regex = "\"artist_name\": \"";
    String artist1_regex = "\"artist_name:1\": \"";
    String artist0_string = "'artist_name': '";
    String artist1_string = "'artist_name:1': '";
    int charsToRemove;

    if (currentLine.contains(artist_regex)) {
      String tempLine = artist0_regex + " ";
      currentLine = currentLine.replaceAll(tempLine, "");

      tempLine = " " + artist0_regex;
      currentLine = currentLine.replaceAll(tempLine, "");
    }
    return currentLine;
  }


  private static String parseArtistName(String currentLine) {
    currentLine = currentLine.replace("\"artist_name\": \"", "")
        .replace("\"artist_name:1\": \"", "")
        .replace("\"artist_name:2\": \"", "")
        .replace("\"artist_name:3\": \"", "");

    currentLine = currentLine.substring(0, currentLine.length()-2);
    return currentLine;
  }
  private static String parseTrackTitle(String currentLine) {
    currentLine = currentLine.replace("\"title\": \"", "");
    currentLine = currentLine.substring(0, currentLine.length()-1)
        .replaceAll("\"", "")
        .replaceAll(",", "");
    return currentLine;
  }

  private static String parseUri(String currentLine) {
    currentLine = currentLine
        .replace("\"uri\": \"", "")
        .replace("\",", "") // gets rid of end commas
        .replaceAll("\\s", ""); // TODO gets rid of whitespace DO NOT include in Artist Name and Title.
    return currentLine;
  }


  public static List<String> readFileInList(String fileName) {
    List<String> lines = Collections.emptyList();
    try {
      lines = Files.readAllLines(Paths.get(fileName), StandardCharsets.UTF_8);
    } catch (IOException e) {
      // do something
      e.printStackTrace();
    }
    return lines;
  }

  public static void main(String[] args) {
    String rawJsonDirectory = "/Users/mkirwin/Spotify/radio-csv/src/main/java/files/";
    final Map<String, List<Track>> fileTrackListMap = getFileToTrackMap(rawJsonDirectory);
    // For each .txt file, generate a csv file using the List<Track> we have generated.
    for (String filename : fileTrackListMap.keySet()) {
      System.out.println("FILENAME:" + filename);
      String csvFilename = filename.replace(".txt", ".csv");
      File currentCsvFile = new File(csvFilename);
      System.out.println("CSV FILENAME:" + currentCsvFile.getPath());

      List<Track> trackList = fileTrackListMap.get(filename);
      int count = 1;

      String FILE_HEADER = "#,Track Title,URI,Artist(s),NUM_ARTISTS";
      String NEW_LINE_SEPARATOR = "\n";
      FileWriter csvFileWriter = null;

      try {
        csvFileWriter = new FileWriter(csvFilename);

        //Write the CSV file header
        csvFileWriter.append(FILE_HEADER.toString());

        //Add a new line separator after the header
        csvFileWriter.append(NEW_LINE_SEPARATOR);
        for (Track track : trackList) {
          //String csvLine = Integer.toString(count++) + "," + track.trackTitle + "," + track.trackUri + "," + String.join("", track.artists);
          String csvLine =
              Integer.toString(count++) + ","
              + track.trackTitle + ","
              + track.trackUri + ","
              + String.join(";", track.artists)
                  .replaceAll("\"", "") + ","
              + track.artists.size();

          //Write a new track object to the file (one line)
          csvFileWriter.append(csvLine);
          csvFileWriter.append(NEW_LINE_SEPARATOR);

          System.out.println("CSV LINE: " + csvLine);
        }


      } catch (IOException e) {
        e.printStackTrace();
      } finally {
        if (csvFileWriter != null) {
          try {
            csvFileWriter.flush();
            csvFileWriter.close();
          } catch (IOException e) {
            e.printStackTrace();
          }
        }
      }


    }

  }



  private static Map<String, List<Track>> getFileToTrackMap(String directory) {
    Map<String, List<Track>> fileTrackListMap = new HashMap<>();
    try {
      Files.walk(Paths.get(directory))
          .filter(Files::isRegularFile)
          .forEach(file -> {
            String filepath = file.toString();
            List<Track> trackList = getTrackListFromFilepath(filepath);
            fileTrackListMap.put(filepath, trackList);
          });
    } catch (IOException e) {
      e.printStackTrace();
    }
    fileTrackListMap.size();
    return fileTrackListMap;
  }

  private static List<Track> getTrackListFromFilepath(final String filepath) {
    String filepath_test = "/Users/mkirwin/Spotify/radio-output/src/com/spotify/files/"
                           + "maddie-maddiewhiterose-legacy.txt";
    List<String> fileLineList =
        readFileInList(filepath);

    Iterator<String> iter = fileLineList.iterator();
    List<Track> trackList = new ArrayList<>();
    List<String> textBuffer = new ArrayList<>();
    int uriCount = 0;
    while (iter.hasNext()) {
      /**
       * Each will contain
       * - uri
       * - artist_name (LIST)
       * - title
       */
      String currentLine = iter.next().trim();
      /**
       * 1. If it's a URI line, then start adding these lines to textBuffer
       * 2. If it's not a URI line, keep adding to textBuffer
       * 3. If it's a URI line, then add to a new textBuffer.
       */
      System.out.println(currentLine); //  todo remove testing line
      if (currentLine.startsWith("\"uri\": \"spotify:track:")) {
        uriCount++;
        if (!textBuffer.isEmpty()) {
          // Get the track information from the last few lines.
          Track previousTrack = parseTrackFromTextSnippet(textBuffer);
          // Add the Track to the list.
          trackList.add(previousTrack);
        }
        // You've encountered another "uri" line and you're not at the beginning. Clear the buffer.
        textBuffer = new ArrayList<>();
      }
      // If you're starting a new track (have hit "uri" line), or not, add this line to the buffer.

      if (!currentLine.isEmpty()) {
        textBuffer.add(currentLine);
      }
      /*
      while (!currentLine.equals("")) {
        String lineToPrint = parseArtistName(iter.next());
        System.out.println(lineToPrint);
      }
       */
    }
    Track finalTrack = parseTrackFromTextSnippet(textBuffer);
    trackList.add(finalTrack);
    System.out.println((trackList.size() == uriCount) +  "//  track count: " + uriCount);
    return trackList;
  }
}
