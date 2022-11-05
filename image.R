rm(list = ls())

library("av")
library("stringr")
library("png")
library("figpatch")
library("animation")

frame_extraction = function(input,outdir){
  pad_frames = c()
  pad_frames_path = c()
  # extract frame
  av_video_images(input, destdir = outdir, format = "png", fps = NULL)
  # file rename and sort
  frames = list.files(outdir)
  for( i in 1:length(frames)){
    pad_frames[i] = str_pad(frames[i],8,side = "left", "0")
    pad_frames_path[i] = paste(outdir,"/",pad_frames[i],sep = "")
    file.rename(paste(outdir,"/",frames[i],sep = ""),pad_frames_path[i])
  }
  frames_path = sort(pad_frames_path)
  return(frames_path)
}

rgb2grey = function(frames,outdir){
  for(x in 1:length(frames)){
    if(x%%100 == 0){
      print(paste("this is loading, frame:",x))
    }
    frame_source = frames[x]
    a = readPNG(source = frame_source)
    #trans rgb to grep
    b <- 255*((a[,,1]*30+a[,,2]*59+a[,,3]*11)+50)/100

    depth <- 5
    #gradient in x and y
    gradx <- matrix(nrow = nrow(b),ncol = ncol(b))
    grady <- matrix(nrow = nrow(b),ncol = ncol(b))
    for (i in 3:ncol(b)) {
      gradx[,i-1] <- (b[,i]-b[,i-2])/2
    }
    gradx[,1] <- b[,2]-b[,1]
    gradx[,ncol(gradx)] <- b[,ncol(b)]-b[,ncol(b)-1]

    for (i in 3:nrow(b)) {
      grady[i-1,] <- (b[i,]-b[i-2,])/2
    }
    grady[1,] <- b[2,]-b[1,]
    grady[nrow(grady),] <- b[nrow(b),]-b[nrow(b)-1,]

    gradx <- gradx*depth/100
    grady <- grady*depth/100

    # beam direction
    vec_el <- pi/2
    vec_az <- pi/4
    dx <- cos(vec_el)*cos(vec_az)/sin(vec_el)
    dy <- cos(vec_el)*sin(vec_az)/sin(vec_el)
    dz <- 1

    #normalization
    A <- gradx^2 + grady^2 +1
    unix <- gradx^2/A
    uniy <- grady^2/A
    uniz <- 1/A

    #sum
    b = (dx*unix+dy*uniy+dz*uniz)
    pad_x = str_pad(x,4,side = "left", "0")
    writePNG(b,target = paste(outdir,"/",pad_x,".png",sep = ""))
  }
  # return()
}


encode_video = function(input_dir,output_file,audio_source){
  # frame_path load
  new_frames_path = c()
  new_frames = list.files(input_dir)
  for( i in 1:length(new_frames)){
    new_frames_path[i] = paste(input_dir,"/",new_frames[i],sep = "")
  }
  
  # video generation
  av_encode_video(
    new_frames_path,
    output = output_file,
    framerate = 25,
    vfilter = "null",
    codec = NULL,
    audio = audio_source,
    verbose = TRUE
  )
  
  # gif generation
  fig(new_frames_path[1])
}


encode_gif = function(input_dir,output_file){
  # frame_path load
  new_frames_path = c()
  new_frames = list.files(input_dir)
  for( i in 1:length(new_frames)){
    new_frames_path[i] = paste(input_dir,"/",new_frames[i],sep = "")
  }
  saveGIF({for(i in 1:200){print(fig(new_frames_path[i]))}},movie.name='ikun.gif',interval=0.04)
  # saveGIF({for(i in 1:length(new_frames_path)){print(fig(new_frames_path[i]))}},movie.name='ikun.gif',interval=0.04,ani.width=1920,ani.height=1080)
}


input_file = "test_cxk.mp4"
initial_frames_dir = "initial_frames"
transed_frames_dir = "transed_frames"
output_file = "ikun.mp4"
output_gif = "ikun.gif"


if (!(dir.exists(initial_frames_dir))){
  dir.create(initial_frames_dir)
}
if (!(dir.exists(transed_frames_dir))){
  dir.create(transed_frames_dir)
}


initial_frames = frame_extraction(input_file,initial_frames_dir)
rgb2grey(initial_frames,transed_frames_dir)
encode_video(transed_frames_dir,output_file,input_file)
encode_gif(transed_frames_dir,output_gif)




