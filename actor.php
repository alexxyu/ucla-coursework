<!DOCTYPE html>
<html>
  <body>
    <?php
      $db = new mysqli('localhost', 'cs143', '', 'class_db');
      if ($db->connect_errno > 0) {
        die('Unable to connect to database [' . $db->connect_error . ']');
      }
      
      $id = $_GET['id'];
      
      // Get and display the actor's name
      $statement = $db->prepare("SELECT first, last FROM Actor WHERE id=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($first, $last);
      $statement->fetch();
      echo '<h1>' . $first . ' ' . $last . '</h1>';
      $statement->close();

      // Create table of all movies that the actor has appeared in
      echo '<table><tr><th>Movies</th></tr>';

      $statement = $db->prepare("SELECT mid, title FROM Movie INNER JOIN MovieActor ON id=mid WHERE aid=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($mid, $title);

      while($statement->fetch()) {
        echo '<tr><td><a href=/movie.php?id=' . $mid .'>' . $title . '</a></td></tr>';
      }
      $statement->close();

      echo '</table>';

      $db->close();
    ?>
  </body>
</html>