% match parent invasive with surface growth dataset
% 
% Kai Li
% 25 Mar 2026

clear
clc

load("invasive_A4.mat");
load("invasive_A6.mat");
load("surface_A4.mat");
load("surface_A6.mat");

%% EXPA_Day_4: match data to get outliers

cc = 1;
for i = 1:length(invasive_A4)
    tmp_name = invasive_A4{i,2}(1:end-1);
    if ~contains(tmp_name,surface_A4(:,2))
        disp(tmp_name)
%         rm_idx_A4(cc) = i;
%         cc = cc+1;
    end
end

%
for i = 1:length(surface_A4)
    tmp_name = surface_A4{i,2}+"w";
    if ~contains(tmp_name,invasive_A4(:,2))
        disp(tmp_name)
    end
end

% remove mismatched files

if length(invasive_A4) == 165
    invasive_A4(rm_idx_A4,:) = [];
end


%% EXPA_Day_6: match data to get outliers
cc = 1;
for i = 1:length(invasive_A6)
    tmp_name = invasive_A6{i,2}(1:end-1);
    if ~contains(tmp_name,surface_A6(:,2))
        disp(tmp_name)
        rm_idx_A6(cc) = i;
        cc = cc+1;
    end
end

% remove row 19 due to replicate 
rm_idx_A6(cc) = 19;

for i = 1:length(surface_A6)
    tmp_name = surface_A6{i,2}+"w";
    if ~contains(tmp_name,invasive_A6(:,2))
        disp(tmp_name)
    end
end

if length(invasive_A6) == 230
    invasive_A6(rm_idx_A6,:) = [];
end

%% EXPA_Day_4: tabulate data

NA4 = length(invasive_A4);

tab_A4 = cell(NA4,11);
name_list = ["alr","ato","dur","fui","mi","msb","tpo"];

for i = 1:NA4

    % 1. full name or ID
    if contains(invasive_A4{i,2},name_list)
        tab_A4{i,1} = invasive_A4{i,2}(5:7);
    else
        tab_A4{i,1} = "parent";
    end
    
    % 2. suphide (Y/N)
    if contains(invasive_A4{i,2},'400S')
        tab_A4{i,2} = 'Y';
    else
        tab_A4{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_A4(:,2),invasive_A4{i,2}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_A4{tmp_idx,1};
    tab_A4{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_A4{i,1};
    tab_A4{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_A4{i,5} = tab_A4{i,4}/tab_A4{i,3};
    
    % 6. Time of trial 
    tab_A4{i,6} = "Dec_03";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_A4{i,2},'BD50')
        tab_A4{i,7} = "BD50";
    elseif contains(invasive_A4{i,2},'BD75')
        tab_A4{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    if contains(invasive_A4{i,2},'1xSLAD')
        tab_A4{i,8} = "1xSLAD";
    elseif contains(invasive_A4{i,2},'2xSLAD')
        tab_A4{i,8} = "2xSLAD";
    end 
    
    % 9. Number of days: 3,4 or 6
    tab_A4{i,9} = "day4";
    
    % 10. Experiment type: A,B,C,D or E
    tab_A4{i,10} = "ExpA";

    % 11. Full name
    tab_A4{i,11} = invasive_A4{i,2}(1:end-1);
end

%% EXPA_Day_4: concantenate to a single table 

Id = 1:length(tab_A4);

df_A4 = table;

df_A4.yeast_id = Id';
df_A4.mutant_id = tab_A4(:,1);
df_A4.sulfide = tab_A4(:,2);
df_A4.surface_area = tab_A4(:,3);
df_A4.washed_area = tab_A4(:,4);
df_A4.area_ratio = tab_A4(:,5);
df_A4.time_of_exp = tab_A4(:,6);
df_A4.nutrient = tab_A4(:,7);
df_A4.slad = tab_A4(:,8);
df_A4.day = tab_A4(:,9);
df_A4.exp_type = tab_A4(:,10);
df_A4.full_name = tab_A4(:,11);

%% EXPA_Day_4: write table to csv

% writetable(df_A4,'../raw_data/image_data/slad_expA_day4.csv')


%% EXPA_Day_6: tabulate data

NA6 = length(invasive_A6);

tab_A6 = cell(NA6,11);

for i = 1:NA6

    % 1. full name or ID
    if contains(invasive_A6{i,2},'796')
        tab_A6{i,1} = "parent";
    else
        tab_A6{i,1} = invasive_A6{i,2}(1:3);
    end

    % 2. suphide (Y/N)
    if contains(invasive_A6{i,2},'400S')
        tab_A6{i,2} = 'Y';
    else
        tab_A6{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_A6(:,2),invasive_A6{i,2}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_A6{tmp_idx,1};
    tab_A6{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_A6{i,1};
    tab_A6{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_A6{i,5} = tab_A6{i,4}/tab_A6{i,3};
    
    % 6. Time of trial 
    tab_A6{i,6} = "Dec_05";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_A6{i,2},'BD50')
        tab_A6{i,7} = "BD50";
    elseif contains(invasive_A6{i,2},'BD75')
        tab_A6{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    if contains(invasive_A6{i,2},'1xSLAD')
        tab_A6{i,8} = "1xSLAD";
    elseif contains(invasive_A6{i,2},'2xSLAD')
        tab_A6{i,8} = "2xSLAD";
    end 
    
    % 9. Number of days: 3,4 or 6
    tab_A6{i,9} = "day6";
    
    % 10. Experiment type: A,B,C,D or E
    tab_A6{i,10} = "ExpA";

    % 11. Full name
    tab_A6{i,11} = invasive_A6{i,2}(1:end-1);
end

%% EXPA_Day_6: concantenate to a single table 

Id = 1:length(tab_A6);

df_A6 = table;

df_A6.yeast_id = Id';
df_A6.mutant_id = tab_A6(:,1);
df_A6.sulfide = tab_A6(:,2);
df_A6.surface_area = tab_A6(:,3);
df_A6.washed_area = tab_A6(:,4);
df_A6.area_ratio = tab_A6(:,5);
df_A6.time_of_exp = tab_A6(:,6);
df_A6.nutrient = tab_A6(:,7);
df_A6.slad = tab_A6(:,8);
df_A6.day = tab_A6(:,9);
df_A6.exp_type = tab_A6(:,10);
df_A6.full_name = tab_A6(:,11);

%% EXPA_Day_6: write table to csv

% writetable(df_A6,'../raw_data/image_data/slad_expA_day6.csv')
