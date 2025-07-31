# Estimate the sprint parameter of lizards in R

This R program is used for estimating sprint parameters after marking body coordinates in DeepLabCut.
Estimated parameters could be classified into three different classes: performance parameters, lateral undulation parameters, and step parameters.

## Performance parameters
- speed, framewised speed.
- acceleration, framewised acceleration.
- sprint_distance, the distance from the first average body point and the last one.
- path_length, sum of frame-wised displacement of the average body point.
- <img width="947" height="439" alt="3 1_v_t_select_all" src="https://github.com/user-attachments/assets/a9562007-2f1b-400f-a7c0-51a81db637af" />


## Lateral undulation parameters
- amplitude_head, the shortest distance from head to sprint axis.
- amplitude_shoulder, the shortest distance from shoulder to sprint axis.
- amplitude_body, the shortest distance from body to sprint axis.
- amplitude_hip, the shortest distance from hip to sprint axis.
- amplitude_tail, the shortest distance from tail to sprint axis.
- spine_angle, framewised angle between the shoulder and body vector and the body and hip vector.
- tail_angle, framewised angle between the shoulder and hip vector and the hip and tail vector.
- head_sprint_angle, framewised angle between the shoulder and head vector and the sprint axis vector.
- body_sprint_angle, framewised angle between the shoulder and hip vector and the sprint axis vector.
- tail_sprint_angle, framewised angle between the tail and hip vector and the sprint axis vector.
- <img width="865" height="582" alt="3 1_trajectory_select_github" src="https://github.com/user-attachments/assets/fcb57ac3-a2ce-46c9-8c1c-7e2a0891f1ae" />

## Step parameters
- step_rate_LF
- step_length_LF
- limb_angle_LF
- step_rate_RF
- step_length_RF
- limb_angle_RF
- step_rate_LH
- step_length_LH
- limb_angle_LH
- step_rate_RH
- step_length_RH
- limb_angle_RH
- <img width="1889" height="3779" alt="3 3_plot_velocity_limb" src="https://github.com/user-attachments/assets/bde003d6-5d06-40d8-9146-1cb7ca00ba58" />
