<!DOCTYPE html>
<html>
  <body>
    <?php
      if(!isset($_GET['id'])) {
        die('Error: invalid request');
      }
      
      $id = $_GET['id'];

      $db = new mysqli('localhost', 'cs143', '', 'class_db');
      if ($db->connect_errno > 0) {
        die('Unable to connect to database [' . $db->connect_error . ']');
      }
      
      // Get and display the movie's information
      $statement = $db->prepare("SELECT title, year, rating, company FROM Movie WHERE id=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($title, $year, $rating, $company);
      $statement->fetch();
      echo '<h1>' . $title . ' ' . $year . ' ' . $rating . ' ' . $company . '</h1>';
      $statement->close();

      // Create table of all actors that were in the movie
      echo '<table><tr><th>Actors</th></tr>';

      $statement = $db->prepare("SELECT aid, first, last FROM Actor INNER JOIN MovieActor ON id=aid WHERE mid=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($aid, $first, $last);

      while($statement->fetch()) {
        echo '<tr><td><a href=/actor.php?id=' . $aid .'>' . $first . ' ' . $last . '</a></td></tr>';
      }
      $statement->close();

      echo '</table>';

      $db->close();
    ?>
  </body>
</html>
