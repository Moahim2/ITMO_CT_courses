package hw.hw8base;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.os.Parcel;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.View;

import androidx.annotation.ColorInt;
import androidx.annotation.Nullable;


public class MyCustomView extends View {
    private final Paint paint1 = new Paint();
    private final Paint paint2 = new Paint();
    private final Paint paint3 = new Paint();
    private int[] alps = new int[] {50, 180, 180};
    private final long delay;


    public MyCustomView(Context context)  {
        this(context, null);
    }

    public MyCustomView(Context context, @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public MyCustomView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        this(context, attrs, defStyleAttr, 0);
    }

    public MyCustomView(Context context, @Nullable AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        TypedArray attributes = context.obtainStyledAttributes(attrs, R.styleable.MyCustomView,
                defStyleAttr, defStyleRes);
        @ColorInt int color;
        try {
            color = attributes.getColor(R.styleable.MyCustomView_color, Color.RED);
            delay = attributes.getInt(R.styleable.MyCustomView_delay, 200);
        } finally {
            attributes.recycle();
        }
        paint1.setColor(color);
        paint2.setColor(color);
        paint3.setColor(color);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        int xm = getWidth() / 2;
        int y = getHeight() / 2;
        int xl = xm - 80;
        int xr = xm + 80;
        canvas.save();

        canvas.drawCircle(xl, y, 40, paint1);
        canvas.drawCircle(xm, y, 40, paint2);
        canvas.drawCircle(xr, y, 40, paint3);
        canvas.restore();

        postDelayed(this::nextFrame, delay);
    }

    private void nextFrame() {
        paint1.setAlpha(alps[0]);
        paint2.setAlpha(alps[1]);
        paint3.setAlpha(alps[2]);
        if (alps[0] == 50) {
            alps[0] = 180;
            alps[1] = 50;
        } else if (alps[1] == 50) {
            alps[1] = 180;
            alps[2] = 50;
        } else {
            alps[2] = 180;
            alps[0] = 50;
        }
        invalidate();
    }

    private void removeLastUpdate() {
        if (alps[0] == 50) {
            alps[0] = 180;
            alps[2] = 50;
        } else if (alps[1] == 50) {
            alps[1] = 180;
            alps[0] = 50;
        } else {
            alps[2] = 180;
            alps[1] = 50;
        }
    }

    @Nullable
    @Override
    protected Parcelable onSaveInstanceState() {
        removeLastUpdate();
        return new MySaver(super.onSaveInstanceState(), alps);
    }

    @Override
    protected void onRestoreInstanceState(Parcelable state) {
        MySaver mySaver = (MySaver) state;
        super.onRestoreInstanceState(mySaver.getSuperState());
        this.alps = mySaver.alps;
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        paint1.setAlpha(alps[0]);
        paint2.setAlpha(alps[1]);
        paint3.setAlpha(alps[2]);
    }
}




class MySaver extends View.BaseSavedState implements Parcelable {
    int[] alps;

    public MySaver(Parcelable superState, int[] curAlps) {
        super(superState);
        this.alps = curAlps;
    }

    protected MySaver(Parcel in) {
        super(in);
        alps = in.createIntArray();
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeIntArray(alps);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public static final Creator<MySaver> CREATOR = new Creator<MySaver>() {
        @Override
        public MySaver createFromParcel(Parcel in) {
            return new MySaver(in);
        }

        @Override
        public MySaver[] newArray(int size) {
            return new MySaver[size];
        }
    };
}

