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

      // Compute the average score the movie recieved
      $statement = $db->prepare("SELECT AVG(rating) FROM Review WHERE mid=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($average_score);
      $statement->fetch();

      // If there are no ratings, than the MySQL query returns NULL
      echo '<h3>Average Score: ' . ($average_score ? $average_score : 'no ratings') . '</h3>';
      $statement->close();

      // Create table of all actors that were in the movie
      echo '<table><tr><th>Actors</th></tr>';

      $statement = $db->prepare("SELECT aid, first, last FROM Actor INNER JOIN MovieActor ON id=aid WHERE mid=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($aid, $first, $last);
      $statement->fetch();

      while($statement->fetch()) {
        echo '<tr><td><a href=actor.php?id=' . $aid .'>' . $first . ' ' . $last . '</a></td></tr>';
      }
      $statement->close();

      // Create a list of all user reviews
      $statement = $db->prepare("SELECT name, rating, comment FROM Review WHERE mid=?");
      $statement->bind_param('i', $id);
      $statement->execute();
      $statement->bind_result($user_name, $user_rating, $user_comment);

      echo '<h3>User Reviews</h3><ul>';
      while ($statement->fetch()) {
        echo '<li><p>' . $user_name . ' gave a review of ' . $user_rating . ' / 5:</p><p>' . $user_comment . '</p>' . '</li>';
      }
      echo '</ul>';
      echo '<a href="review.php?id=' . $id . '">Add Review</a>';

      $statement->close();

      echo '</table>';

      $db->close();
    ?>
  </body>
</html>
