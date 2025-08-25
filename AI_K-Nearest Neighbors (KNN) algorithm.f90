program knn_classifier
  implicit none

  integer, parameter :: num_samples = 6
  integer, parameter :: num_features = 2
  integer, parameter :: k = 3
  real :: training_data(num_samples, num_features)
  integer :: training_labels(num_samples)
  real :: test_point(num_features)
  integer :: prediction

  ! Sample training data (each row is a data point with 2 features)
  data training_data /1.0, 2.0, 2.0, 3.0, 3.0, 3.0, 5.0, 1.0, 6.0, 5.0, 7.0, 3.0/
  
  ! Corresponding labels for the training data (0 or 1)
  data training_labels /0, 0, 1, 1, 0, 1/

  ! Define the test point
  test_point = (/4.0, 3.0/)

  ! Predict the class of the test point using KNN.
  prediction = knn_predict(test_point, training_data, training_labels, k)
  
  ! Output the prediction
  print *, "Predicted class for test point (", test_point(1), ",", test_point(2), "): ", prediction

contains

  ! Function to predict the class of a point using K-Nearest Neighbors.
  integer function knn_predict(test_point, training_data, training_labels, k)
    real, intent(in) :: test_point(num_features)
    real, intent(in) :: training_data(num_samples, num_features)
    integer, intent(in) :: training_labels(num_samples)
    integer, intent(in) :: k
    real :: distances(num_samples)
    integer :: nearest_labels(k)
    integer :: i, j

    ! Calculate distances from the test point to each training point
    do i = 1, num_samples
      distances(i) = 0.0
      do j = 1, num_features
        distances(i) = distances(i) + (test_point(j) - training_data(i, j))**2
      end do
      distances(i) = sqrt(distances(i))  ! Euclidean distance
    end do

    ! Find the labels of the K nearest neighbors
    call find_nearest_neighbors(distances, training_labels, k, nearest_labels)
    
    ! Predict the class by majority vote of the nearest neighbors
    knn_predict = majority_vote(nearest_labels)
  end function knn_predict

  ! Subroutine to find the K nearest neighbors
  subroutine find_nearest_neighbors(distances, training_labels, k, nearest_labels)
    real, intent(in) :: distances(num_samples)
    integer, intent(in) :: training_labels(num_samples)
    integer, intent(out) :: nearest_labels(k)
    integer :: sorted_indices(num_samples)
    integer :: i, j, temp

    ! Initialize indices
    do i = 1, num_samples
      sorted_indices(i) = i
    end do

    ! Sort indices by distance (selection sort for simplicity)
    do i = 1, num_samples - 1
      do j = i + 1, num_samples
        if (distances(sorted_indices(i)) > distances(sorted_indices(j))) then
          temp = sorted_indices(i)
          sorted_indices(i) = sorted_indices(j)
          sorted_indices(j) = temp
        end if
      end do
    end do

    ! Get the labels of the K nearest neighbors
    do i = 1, k
      nearest_labels(i) = training_labels(sorted_indices(i))
    end do
  end subroutine find_nearest_neighbors

  ! Function to determine the majority class from K labels
  integer function majority_vote(nearest_labels)
    integer, intent(in) :: nearest_labels(k)
    integer :: count_0, count_1, i

    count_0 = 0
    count_1 = 0

    ! Count occurrences of each label (0 or 1)
    do i = 1, k
      if (nearest_labels(i) == 0) then
        count_0 = count_0 + 1
      else if (nearest_labels(i) == 1) then
        count_1 = count_1 + 1
      end if
    end do

    ! Predict the majority class
    if (count_0 > count_1) then
      majority_vote = 0
    else
      majority_vote = 1
    end if
  end function majority_vote

end program knn_classifier
